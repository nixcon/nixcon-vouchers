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
    , merges :: Int
    , special :: Bool
    , voucher :: Maybe Voucher
    }
    deriving stock (Generic, Show, Eq)

instance Monoid Contributor where
    mempty =
        Contributor
            { githubId = 0
            , commits = 0
            , merges = 0
            , special = False
            , voucher = Nothing
            }

instance Semigroup Contributor where
    c1 <> c2 =
        c1
            { commits = c1.commits + c2.commits
            , merges = c1.merges + c2.merges
            , special = c1.special || c2.special
            , voucher = c1.voucher <|> c2.voucher
            }

type Contributors = IntMap Contributor

updateContributor
    :: Contributor
    -> (Contributor -> Contributor)
    -> Contributors
    -> Contributors
updateContributor c f = IntMap.insertWith (const f) c.githubId c

setContributor :: Contributor -> Contributors -> Contributors
setContributor c = updateContributor c $ const c

contributorsFromCsv :: Contributors
contributorsFromCsv = IntMap.unionWith (<>) contributors included
  where
    contributors =
        IntMap.fromList
            . map (\c -> (c.githubId, c))
            . filter
                ( \Contributor{..} ->
                    commits >= 1
                        || merges >= 1
                        || special
                        || isJust voucher
                )
            . mapMaybe parseContributor
            . Text.lines
            . Text.decodeUtf8
            $ $(embedFile =<< makeRelativeToProject "contributors.csv")
    included =
        IntMap.fromList
            . map (\c -> (c.githubId, c))
            . mapMaybe parseIncluded
            . Text.lines
            . Text.decodeUtf8
            $ $(embedFile =<< makeRelativeToProject "included.csv")
    parseContributor :: Text -> Maybe Contributor
    parseContributor
        ( Text.split (== ',') ->
                [ treadMaybe -> Just githubId
                    , _
                    , treadMaybe -> Just commits
                    , treadMaybe -> Just merges
                    ]
            ) =
            Just mempty{githubId, commits, merges}
    parseContributor _ = Nothing
    parseIncluded :: Text -> Maybe Contributor
    parseIncluded (Text.split (== ',') -> [treadMaybe -> Just githubId, _]) =
        Just mempty{githubId, special = True}
    parseIncluded _ = Nothing
    treadMaybe :: (Read a) => Text -> Maybe a
    treadMaybe = readMaybe . Text.unpack
