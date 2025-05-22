{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Contributor
import Control.Concurrent (threadDelay)
import Control.Lens.Operators
import Control.Monad.Extra (maybeM)
import Data.Aeson (FromJSON, ToJSON (toJSON))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy (LazyByteString)
import Data.Foldable qualified as Foldable
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List.Extra qualified as List
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq, ViewR ((:>)))
import Data.Sequence qualified as Seq
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Data.Time (UTCTime (UTCTime), diffUTCTime, fromGregorian, getCurrentTime)
import Data.Traversable (for)
import Network.HTTP.Types (status200)
import Network.Wreq qualified as Wreq
import PullRequests
import Query
import Repositories hiding (Repository)
import Response
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import Text.Pretty.Simple (pHPrint)
import Prelude

startDate :: UTCTime
startDate = UTCTime (fromGregorian 2021 5 1) 0

getToken :: IO Text
getToken =
    lookupEnv "GITHUB_TOKEN"
        & maybeM
            (fail "You really should set GITHUB_TOKEN")
            (pure . fromString)

post' :: (ToJSON a, FromJSON b) => Text -> a -> IO (Either (Wreq.Response LazyByteString) b)
post' token a =
    Wreq.postWith opts "https://api.github.com/graphql" (toJSON a) >>= \case
        response
            | response ^. Wreq.responseStatus == status200
            , Just b <- Aeson.decode $ response ^. Wreq.responseBody ->
                pure . Right $ b
        response -> pure . Left $ response
  where
    opts =
        Wreq.defaults
            & Wreq.auth
                ?~ Wreq.oauth2Token (Text.encodeUtf8 token)
            & Wreq.header "Accept"
                .~ ["application/vnd.github.v3+json"]

post :: forall a. (FromJSON a) => Query -> IO (Response a)
post q = go 0
  where
    go :: Int -> IO (Response a)
    go 10 = fail "API fail"
    go n = do
        token <- getToken
        post' token q >>= \case
            Right r -> pure r
            Left e -> do
                hPutStrLn stderr "ERROR"
                pHPrint stderr e
                threadDelay 1_000_000
                go (n + 1)

main :: IO ()
main = do
    repos <-
        Foldable.fold <$> for ["NixOS", "NixCon"] \owner ->
            post (repositories owner 100 Nothing) <&> \Response{data' = OrganizationData{organization = Organization{repositories = Nodes{nodes}}}} ->
                nodes <&> \repo -> (owner, repo.name)
    prs <- Foldable.fold <$> for repos \(owner, name) -> go owner name Nothing
    let commitsByContributor :: HashMap (Maybe Contributor) Int
        commitsByContributor = HashMap.fromListWith (+) . Foldable.toList $ prs <&> \pr -> (pullRequestContributor pr, pr.commits.totalCount)
        sortedContributors = List.sortOn (negate . snd) . mapMaybe (\(k, v) -> k <&> (,v)) $ HashMap.toList commitsByContributor
    Text.writeFile "contributors.csv" . Text.unlines $
        toRow ["githubId", "githubUsername", "commits"] : (contributorRow <$> sortedContributors)
  where
    toRow = Text.intercalate ","
    contributorRow :: (Contributor, Int) -> Text
    contributorRow (Contributor{..}, commits) = toRow $ fromString <$> [show githubId, Text.unpack githubUsername, show commits]
    go :: Text -> Text -> Maybe Text -> IO (Seq PullRequest)
    go owner name before = do
        time <- getCurrentTime
        Response{data' = Repository{pullRequests = Nodes{..}}} <- post $ pullRequests owner name 100 before
        dt <- diffUTCTime <$> getCurrentTime <*> pure time
        case Seq.viewr nodes of
            _ :> pr | pr.createdAt > startDate -> do
                pHPrint stderr (pr.createdAt, dt)
                (<> nodes) <$> go owner name (pageInfo <&> (.startCursor))
            _ -> do
                pHPrint stderr dt
                pure nodes
