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
    , githubUsername :: Text
    , commits :: Int
    , voucher :: Maybe Voucher
    }
    deriving stock (Generic, Show, Eq)

type Contributors = IntMap Contributor

contributorsFromCsv :: Int -> Contributors
contributorsFromCsv minimumCommits =
    IntMap.fromList
        . filter (\(_, Contributor{commits}) -> commits >= minimumCommits)
        . mapMaybe parseRow
        . Text.lines
        . Text.decodeUtf8
        $ $(embedFile =<< makeRelativeToProject "contributors.csv")
  where
    parseRow :: Text -> Maybe (Int, Contributor)
    parseRow (Text.split (== ',') -> [treadMaybe -> Just githubId, githubUsername, treadMaybe -> Just commits]) =
        Just (githubId, Contributor{voucher = Nothing, ..})
    parseRow _ = Nothing
    treadMaybe :: (Read a) => Text -> Maybe a
    treadMaybe = readMaybe . Text.unpack
