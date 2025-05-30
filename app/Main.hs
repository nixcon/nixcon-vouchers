{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-orphans #-}

module Main (main) where

import API (Routes (Routes))
import CliArgs (getConfig)
import Config (Config (..), GithubConfig (..), PretixConfig (..), writeConfigFile)
import Contributor
import Control.Monad.Error.Class qualified as MonadError
import Data.CaseInsensitive qualified as CI
import Data.Foldable (for_)
import Data.HashMap.Strict qualified as HashMap
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (getCurrentTime)
import Data.Tuple (swap)
import Effectful.Servant (interpretServer)
import Effectful.Servant.Generic (runWarpServerSettingsContext)
import Effectful.Wreq (runWreq)
import Log.Backend.StandardOutput (withStdOutLogger)
import Network.HTTP.Types (ResponseHeaders, Status, statusCode)
import Network.HTTP.Types.Header (HeaderName)
import Network.HTTP.Types.URI (queryToQueryText)
import Network.Wai
import Network.Wai.Handler.Warp (Settings, defaultSettings, setHost, setPort)
import Network.Wai.Internal (Response (..))
import Network.Wai.Middleware.Auth.OAuth2 (OAuth2 (..))
import Network.Wai.Middleware.Auth.OAuth2.Github (Github, mkGithubProvider)
import Network.Wai.Middleware.Auth.Provider (ProviderInfo (..))
import OAuth2 (Github (..), mkGithubSettings)
import Pretix (Voucher (..), genVoucherCode, getAllVouchers)
import Servant hiding (throwError)
import Servant.OAuth2 (oauth2AuthHandler)
import Servant.OAuth2.Cookies (getSessionIdFromCookie)
import Servant.Server (pattern MkHandler)
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import Servant.Server.Generic (AsServerT)
import Server (Env (..), optionalSessionHandler, requiredSessionHandler, server)
import System.Exit (exitFailure)
import Web.ClientSession (Key, getDefaultKey)
import Prelude

showBs :: ByteString -> Text
showBs = either (Text.pack . show) id . Text.decodeUtf8'

logMiddleware :: LoggerEnv -> Application -> Application
logMiddleware loggerEnv app req respond' = do
    time <- getCurrentTime
    logMessageIO loggerEnv time LogTrace "Request" $
        object
            [ "method" .= showBs req.requestMethod
            , "query"
                .= object
                    [ (fromString . Text.unpack $ name) .= value
                    | (name, value) <- queryToQueryText req.queryString
                    ]
            , "url" .= showBs req.rawPathInfo
            , "remote_host" .= show req.remoteHost
            , "user_agent" .= fmap showBs req.requestHeaderUserAgent
            , "body_length" .= show req.requestBodyLength
            , "headers"
                .= object
                    [ (fromString . Text.unpack . Text.decodeUtf8 . CI.original $ name)
                        .= Text.decodeUtf8 value
                    | (name, value) <- req.requestHeaders
                    ]
            ]
    app req \res -> do
        logMessageIO loggerEnv time LogTrace "Response" $
            object
                [ "status" .= statusCode (responseStatus res)
                , "headers"
                    .= object
                        [ (fromString . Text.unpack . Text.decodeUtf8 . CI.original $ name)
                            .= Text.decodeUtf8 value
                        | (name, value) <- responseHeaders res
                        ]
                ]
        respond' res

instance (Show e) => MonadError e (Eff (Error e ': es)) where
    throwError = throwError
    catchError e = catchError e . const

main :: IO ()
main = do
    Config
        { port = Identity port
        , logLevel = Identity logLevel
        , githubConfig = Identity GithubConfig{..}
        , pretixConfig = Identity pretixConfig
        , sessionKey = Identity sessionKey
        , minimumCommits = Identity minimumCommits
        } <- withStdOutLogger \logger -> runEff . runLog "nixcon-vouchers" logger LogInfo $ getConfig
    let githubSettings =
            mkGithubSettings
                sessionKey
                Github
                    { oauth2 =
                        OAuth2
                            { oa2ClientId = clientId
                            , oa2ClientSecret = clientSecret
                            , oa2AuthorizeEndpoint = "https://github.com/login/oauth/authorize"
                            , oa2AccessTokenEndpoint = "https://github.com/login/oauth/access_token"
                            , oa2Scope = Just ["read:user"]
                            , oa2ProviderInfo =
                                ProviderInfo
                                    { providerTitle = "GitHub"
                                    , providerLogoUrl = ""
                                    , providerDescr = "Use your GitHub account to access this page."
                                    }
                            }
                    , callbackUrl
                    }
        initialEnv :: Env
        initialEnv =
            Env
                { githubSettings
                , pretixConfig
                }
    withStdOutLogger \logger -> runEff
        . runLog "nixcon-vouchers" logger logLevel
        . runReader initialEnv
        . evalState (contributorsFromCsv minimumCommits)
        . runTime
        . runWreq
        $ do
            voucherByCode <- HashMap.fromList . fmap (\v -> (v.code, v)) <$> getAllVouchers pretixConfig
            get @Contributors >>= mapM_ \contributor ->
                let code = genVoucherCode contributor.githubId pretixConfig.voucherCodeSalt
                    voucher = HashMap.lookup code voucherByCode
                 in modify . setContributor $ contributor{voucher}

            let settings :: Settings
                settings = setHost "*6" . setPort port $ defaultSettings
                context =
                    requiredSessionHandler sessionKey
                        :. optionalSessionHandler sessionKey
                        :. oauth2AuthHandler githubSettings (MkHandler . runEff . runErrorNoCallStack)
                        :. EmptyContext
            logMessage LogInfo ("Server listening on http://localhost:" <> ishow port) $ object []
            loggerEnv <- getLoggerEnv
            let middleware = logMiddleware loggerEnv
            runWarpServerSettingsContext settings context server middleware
