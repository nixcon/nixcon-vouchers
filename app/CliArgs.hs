module CliArgs where

import Config
import Data.Bifunctor (Bifunctor (first))
import Data.List qualified as List
import Options.Applicative
import System.Directory (doesFileExist)
import Prelude

data CliArgs = CliArgs
    { configFile :: FilePath
    , config :: Config Maybe
    }
    deriving stock (Generic)

parseGithubConfig :: Parser GithubConfig
parseGithubConfig = do
    clientId <- strOption $ long "github-client-id"
    clientSecret <- strOption $ long "github-client-secret"
    callbackUrl <- strOption $ long "github-callback-url"
    pure GithubConfig{..}

parsePretixConfig :: Parser PretixConfig
parsePretixConfig = do
    organizer <- strOption $ long "pretix-organizer"
    event <- strOption $ long "pretix-event"
    apiToken <- strOption $ long "pretix-api-token"
    storeUrl <- option auto $ long "pretix-store-url"
    voucherItem <- option auto $ long "pretix-voucher-item"
    voucherCodeSalt <- strOption $ long "pretix-voucher-code-salt"
    pure PretixConfig{..}

parseConfig :: Parser (Config Maybe)
parseConfig = do
    port <-
        optional . option auto $
            long "port"
                <> short 'p'
                <> metavar "PORT"
                <> help "The port to listen on."
    logLevel <-
        optional
            . option (eitherReader $ readLogLevelEither . fromString)
            $ long "log-level"
                <> metavar "LOG_LEVEL"
                <> help ("Verbosity of the program. Possible values: " <> List.intercalate "," logLevels)
                <> completeWith logLevels
    githubConfig <- optional parseGithubConfig
    pretixConfig <- optional parsePretixConfig
    sessionKey <-
        optional . option (eitherReader $ first fromText . decodeKey . fromString) $
            long "session-key"
                <> metavar "KEY"
                <> help "The key used to encrypt session cookies. Base64-encoded and exactly 96 bytes long."
    pure Config{..}
  where
    logLevels :: [String]
    logLevels =
        [ fromText . showLogLevel $ l
        | l <- [minBound .. maxBound]
        ]

parseCliArgs :: Parser CliArgs
parseCliArgs = do
    configFile <-
        strOption $
            long "config"
                <> short 'c'
                <> metavar "FILE"
                <> help "Path to the configuration file"
                <> value "config.dhall"
                <> showDefaultWith id
    config <- parseConfig
    pure CliArgs{..}

parserInfo :: ParserInfo CliArgs
parserInfo = info (helper <*> parseCliArgs) (fullDesc <> progDesc "ZHF Hello server")

getConfig :: (Log :> es, IOE :> es) => Eff es (Config Identity)
getConfig = do
    CliArgs{..} <- liftIO $ execParser parserInfo
    case maybeConfig config of
        Nothing -> overrideConfig config <$> readConfigFile configFile
        Just c -> do
            unlessM (liftIO $ doesFileExist configFile) (writeConfigFile configFile c)
            pure c
