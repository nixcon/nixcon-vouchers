{-# LANGUAGE TemplateHaskell #-}

module PullRequests where

import Contributor (Contributor (..))
import Control.Applicative (optional)
import Control.Monad (MonadPlus (mzero))
import Data.Aeson (FromJSON (..), (.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.ByteString qualified as ByteString
import Data.ByteString.Base64 (decodeBase64Lenient)
import Data.FileEmbed
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Query
import Response
import Text.Read (readMaybe)
import Prelude

pullRequests :: Text -> Text -> Int -> Maybe Text -> Query
pullRequests owner name count before =
    Query
        { query =
            subst
                [ ("$OWNER", ishow owner)
                , ("$NAME", ishow name)
                , ("$COUNT", ishow count)
                , ("$BEFORE", maybe "null" ishow before)
                ]
                . Text.decodeUtf8
                $ $(embedFile =<< makeRelativeToProject "gen/pullRequests.gql")
        }

pullRequestContributor :: PullRequest -> Maybe Contributor
pullRequestContributor PullRequest{..} =
    author <&> \Author{..} ->
        Contributor
            { githubId = id'
            , githubUsername = login
            }

data Author = Author
    { id' :: Int
    , login :: Text
    }
    deriving stock (Generic, Show, Eq)

parseAuthorId :: Aeson.Value -> Aeson.Parser Int
parseAuthorId =
    Aeson.withText "Author.id" $
        maybe mzero (maybe mzero pure . readMaybe . Text.unpack . Text.decodeUtf8)
            . ByteString.stripPrefix "04:User"
            . decodeBase64Lenient
            . Text.encodeUtf8

instance FromJSON Author where
    parseJSON = Aeson.withObject "Author" \o -> do
        id' <- parseAuthorId =<< o .: "id"
        login <- o .: "login"
        pure Author{..}

data PullRequest = PullRequest
    { number :: Int
    , createdAt :: UTCTime
    , mergedAt :: Maybe UTCTime
    , author :: Maybe Author
    , commits :: TotalCount
    }
    deriving stock (Generic, Show, Eq)

instance FromJSON PullRequest where
    parseJSON = Aeson.withObject "PullRequest" \o -> do
        number <- o .: "number"
        createdAt <- o .: "createdAt"
        mergedAt <- o .: "createdAt"
        author <- optional $ o .: "author"
        commits <- o .: "commits"
        pure PullRequest{..}

newtype Repository = Repository
    { pullRequests :: Nodes PullRequest
    }
    deriving stock (Generic)
    deriving anyclass (FromJSON)
    deriving newtype (Show, Eq)

newtype RepositoryData = RepositoryData
    { repository :: Repository
    }
    deriving stock (Generic)
    deriving anyclass (FromJSON)
    deriving newtype (Show, Eq)
