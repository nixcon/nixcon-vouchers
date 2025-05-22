module OAuth2 where

import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Except (except)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), maybeToExceptT)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Either.Extra (mapLeft)
import Data.Text.Encoding qualified as Text
import Effectful.Wreq (runWreq)
import Effectful.Wreq qualified as Wreq
import Network.HTTP.Types (status200, status501)
import Network.OAuth.OAuth2 (OAuth2Token)
import Network.OAuth.OAuth2 qualified as OA2
import Network.Wai.Auth.Internal (decodeToken)
import Network.Wai.Middleware.Auth.OAuth2 (OAuth2 (..))
import Network.Wai.Middleware.Auth.Provider (AuthProvider (..))
import Servant (Header, Headers, ServerError, WithStatus (..), addHeader, respond)
import Servant.OAuth2 (OAuth2Settings (..), defaultOAuth2Settings)
import Servant.OAuth2.Cookies (buildSessionCookie)
import Session
import Text.Blaze.Html (Html, (!))
import Text.Blaze.Html5 (head, html, meta)
import Text.Blaze.Html5.Attributes (content, httpEquiv)
import Web.ClientSession (Key)
import Web.Cookie (SetCookie)
import Prelude

data Github = Github
    { oauth2 :: OAuth2
    , callbackUrl :: Text
    }

-- Maybe this works if the domain does not contain a port?
-- type OAuth2Result = '[WithStatus 302 RedirectWithCookie]
type OAuth2Result = '[WithStatus 200 (Headers '[Header "Set-Cookie" SetCookie] Html)]

mkGithubSettings :: Key -> Github -> OAuth2Settings (Eff '[Error ServerError, IOE]) Github OAuth2Result
mkGithubSettings key github =
    (defaultOAuth2Settings @(Eff '[Error ServerError, IOE]) github)
        { success = \_req ident -> do
            cookie <- liftIO $ buildSessionCookie key ident
            -- An HTML redirect to / with 0 timeout
            -- <head><meta http-equiv="refresh" content="0; url=/"></head>
            -- Why aren't we using status code 303 with Location header?
            -- Because the browser stores the Set-Cookie for future requests, but *does not redirect with it*.
            respond . WithStatus @200 . addHeader @"Set-Cookie" cookie . html . Text.Blaze.Html5.head $
                meta ! httpEquiv "refresh" ! content "0; url=/"
        }

data GithubUser = GithubUser
    { login :: Text
    , id :: Int
    }
    deriving stock (Generic, Show)

instance ToJSON GithubUser where
    toJSON = genericToJSON Aeson.defaultOptions{Aeson.fieldLabelModifier = Aeson.camelTo2 '_'}

instance FromJSON GithubUser where
    parseJSON = genericParseJSON Aeson.defaultOptions{Aeson.fieldLabelModifier = Aeson.camelTo2 '_'}

-- | Makes an API call to github and retrieves the authenticated user
getUser :: (Wreq :> es) => OAuth2Token -> Eff es (Maybe GithubUser)
getUser accessToken =
    Wreq.getWith opts "https://api.github.com/user" >>= \case
        response | response ^. Wreq.responseStatus == status200 -> pure . Aeson.decode $ response ^. Wreq.responseBody
        _ -> pure Nothing
  where
    opts =
        Wreq.defaults
            & Wreq.auth
            ?~ Wreq.oauth2Token (Text.encodeUtf8 . OA2.atoken . OA2.accessToken $ accessToken)
            & Wreq.header "Accept"
            .~ ["application/vnd.github.v3+json"]

instance AuthProvider Github where
    getProviderName _ = "github"
    getProviderInfo Github{oauth2} = getProviderInfo oauth2
    handleLogin Github{oauth2} req suffix renderUrl onSuccess onFailure = handleLogin oauth2 req suffix renderUrl onOAuth2Success onFailure
      where
        onOAuth2Success oauth2Token =
            either (onFailure status501) (onSuccess . LazyByteString.toStrict . Aeson.encode) =<< runExceptT do
                token <- except . mapLeft (("Error decoding OAuth2 token: " <>) . fromString . show) . decodeToken $ oauth2Token
                user <- maybeToExceptT "Failed to authenticate with GitHub" . MaybeT . runEff . runWreq . getUser $ token
                pure Session{githubId = user.id, githubUsername = user.login}
