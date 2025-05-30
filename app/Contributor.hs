{-# LANGUAGE FieldSelectors #-}
{-# LANGUAGE TemplateHaskell #-}

module Contributor where

import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Pretix (Voucher)
import Text.Read (readMaybe)
import Prelude

data Contributor = Contributor
    { githubId :: Int
    , commits :: Int
    , voucher :: Maybe Voucher
    }
    deriving stock (Generic, Show, Eq)

type Contributors = IntMap Contributor

updateContributor :: Contributor -> (Contributor -> Contributor) -> Contributors -> Contributors
updateContributor c f = IntMap.insertWith (const f) c.githubId c

setContributor :: Contributor -> Contributors -> Contributors
setContributor c = updateContributor c $ const c

contributorsFromCsv :: Int -> Contributors
contributorsFromCsv minimumCommits = IntMap.unionWith (\c1 c2 -> c1{commits = c1.commits + c2.commits}) contributors organisers
  where
    contributors =
        IntMap.fromList
            . map (\c -> (c.githubId, c))
            . filter (\Contributor{commits} -> commits >= minimumCommits)
            . mapMaybe parseContributor
            . Text.lines
            . Text.decodeUtf8
            $ $(embedFile =<< makeRelativeToProject "contributors.csv")
    organisers =
        IntMap.fromList
            . map (\c -> (c.githubId, c))
            . mapMaybe parseOrganiser
            . Text.lines
            . Text.decodeUtf8
            $ $(embedFile =<< makeRelativeToProject "contributors.csv")
    parseContributor :: Text -> Maybe Contributor
    parseContributor (Text.split (== ',') -> [treadMaybe -> Just githubId, _, treadMaybe -> Just commits]) =
        Just Contributor{voucher = Nothing, ..}
    parseContributor _ = Nothing
    parseOrganiser :: Text -> Maybe Contributor
    parseOrganiser (Text.split (== ',') -> [treadMaybe -> Just githubId, _]) =
        Just Contributor{githubId, commits = 0, voucher = Nothing}
    parseOrganiser _ = Nothing
    treadMaybe :: (Read a) => Text -> Maybe a
    treadMaybe = readMaybe . Text.unpack
