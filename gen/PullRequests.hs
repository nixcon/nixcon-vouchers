{-# LANGUAGE TemplateHaskell #-}

module PullRequests where

import Contributor (Contributor (..))
import Data.Aeson (FromJSON (..))
import Data.Aeson qualified as Aeson
import Data.FileEmbed
import Data.Functor ((<&>))
import Data.List.Extra (dropSuffix)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Query
import Response
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
pullRequestContributor PullRequest{author} =
    author <&> \Author{..} ->
        Contributor
            { githubId = maybe id' ishow databaseId
            , githubUsername = login
            }

data Author = Author
    { id' :: Text
    , databaseId :: Maybe Int
    , login :: Text
    }
    deriving stock (Generic, Show, Eq)

instance FromJSON Author where
    parseJSON = Aeson.genericParseJSON Aeson.defaultOptions{Aeson.fieldLabelModifier = dropSuffix "'"}

data PullRequest = PullRequest
    { number :: Int
    , createdAt :: UTCTime
    , mergedAt :: Maybe UTCTime
    , author :: Maybe Author
    , commits :: TotalCount
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromJSON)

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
