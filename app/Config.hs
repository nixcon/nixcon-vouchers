{-# OPTIONS_GHC -Wno-orphans #-}

module Config where

import Data.Base64.Types (extractBase64)
import Data.ByteString.Base64 (decodeBase64Untyped, encodeBase64)
import Data.Either.Extra (mapLeft)
import Data.Either.Validation (eitherToValidation, validationToEither)
import Data.Serialize qualified as Serialize
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Data.Void (Void)
import Dhall hiding (maybe)
import Dhall.Core (Expr)
import Dhall.Pretty
import Dhall.Src (Src)
import Log
import Network.Wai.Handler.Warp (Port)
import Prettyprinter qualified
import Prettyprinter.Render.Text qualified as Prettyprinter
import Web.ClientSession (Key, initKey)
import Prelude

data GithubConfig = GithubConfig
    { clientId :: Text
    , clientSecret :: Text
    , callbackUrl :: Text
    }
    deriving stock (Generic)
    deriving anyclass (FromDhall, ToDhall)

data PretixConfig = PretixConfig
    { organizer :: Text
    , event :: Text
    , apiToken :: Text
    , storeUrl :: Text
    , voucherItem :: Int
    , voucherCodeSalt :: Text
    }
    deriving stock (Generic)
    deriving anyclass (FromDhall, ToDhall)

data Config m = Config
    { port :: m Port
    , logLevel :: m LogLevel
    , githubConfig :: m GithubConfig
    , pretixConfig :: m PretixConfig
    , sessionKey :: m Key
    }
    deriving stock (Generic)

decodeKey :: Text -> Either Text Key
decodeKey = mapLeft fromString . initKey <=< decodeBase64Untyped . Text.encodeUtf8

instance FromDhall Key where
    autoWith norm = decoder{extract}
      where
        decoder :: Decoder Text
        decoder = autoWith norm
        extract :: Expr Src Void -> Extractor Src Void Key
        extract e =
            eitherToValidation $
                mapLeft (DhallErrors . pure . ExtractError) . decodeKey
                    =<< validationToEither (decoder.extract e)

instance ToDhall Key where
    injectWith norm = encoder{embed}
      where
        encoder :: Encoder Text
        encoder = injectWith norm
        embed :: Key -> Expr Src Void
        embed = encoder.embed . encodeKey
        encodeKey :: Key -> Text
        encodeKey = extractBase64 . encodeBase64 . Serialize.encode

deriving anyclass instance FromDhall (Config Identity)

deriving anyclass instance ToDhall (Config Identity)

deriving newtype instance (ToDhall a) => ToDhall (Identity a)

readConfigFile :: (Log :> es, IOE :> es) => FilePath -> Eff es (Config Identity)
readConfigFile path =
    tryLogged
        ("Read config file " <> fromString path)
        ("Failed to read config file " <> fromString path)
        . liftIO
        . inputFile auto
        $ path

writeConfigFile :: (Log :> es, IOE :> es) => FilePath -> Config Identity -> Eff es ()
writeConfigFile path config = do
    liftIO
        . Text.writeFile path
        . Prettyprinter.renderStrict
        . Prettyprinter.layoutPretty Prettyprinter.defaultLayoutOptions
        . prettyCharacterSet Unicode
        . embed Dhall.inject
        $ config
    logMessage LogInfo ("Wrote config file " <> fromString path) $ object []

deriving stock instance Generic LogLevel

instance ToDhall LogLevel

instance FromDhall LogLevel

maybeConfig :: Config Maybe -> Maybe (Config Identity)
maybeConfig config = do
    port <- pure <$> config.port
    logLevel <- pure <$> config.logLevel
    githubConfig <- pure <$> config.githubConfig
    pretixConfig <- pure <$> config.pretixConfig
    sessionKey <- pure <$> config.sessionKey
    pure Config{..}

overrideConfig :: Config Maybe -> Config Identity -> Config Identity
overrideConfig new Config{..} =
    Config
        { port = maybe port pure new.port
        , logLevel = maybe logLevel pure new.logLevel
        , githubConfig = maybe githubConfig pure new.githubConfig
        , pretixConfig = maybe pretixConfig pure new.pretixConfig
        , sessionKey = maybe sessionKey pure new.sessionKey
        }
