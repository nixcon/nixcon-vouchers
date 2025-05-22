{-# OPTIONS_GHC -Wno-orphans #-}

module Pretix where

import Config (PretixConfig (..))
import Crypto.Hash (SHA256, hash)
import Data.Aeson qualified as Aeson
import Data.Base64.Types (extractBase64)
import Data.ByteArray (convert)
import Data.ByteString.Base64 (encodeBase64)
import Data.Char (isAlphaNum, toUpper)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time.Format.ISO8601 (iso8601Show)
import Effectful.Wreq
import Effectful.Wreq qualified as Wreq
import Network.HTTP.Types (status200, status201)
import Session (Session (..))
import Prelude

data Voucher = Voucher
    { id :: Int
    , code :: Text
    , redeemed :: Int
    , comment :: Text
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromJSON, ToJSON)

-- | Probably not safe, do not try this at home! 🤭
fakeBase36 :: ByteString -> Text
fakeBase36 = Text.filter isAlphaNum . Text.map toUpper . extractBase64 . encodeBase64

{- | Generates a voucher code with the format: xxxxx-xxxxx-xxxxx-xxxx
The code is hard to guess, but always the same for the same input.
This guarantee prevents timing attacks that could generate multiple vouchers.
-}
genVoucherCode :: PretixConfig -> Int -> Text
genVoucherCode PretixConfig{..} githubId =
    Text.intercalate "-"
        . Text.chunksOf 5
        . Text.take 20
        . fakeBase36
        . convert
        . hash @ByteString @SHA256
        . fromText
        $ voucherCodeSalt <> ishow githubId

createNewVoucher :: (Wreq :> es, Time :> es) => PretixConfig -> Session -> Eff es (Maybe Voucher)
createNewVoucher pretixConfig@PretixConfig{..} Session{..} = do
    time <- currentTime
    Wreq.postWith opts url (payload time) >>= \case
        response
            | response ^. Wreq.responseStatus == status201 ->
                pure $ Aeson.decode (response ^. Wreq.responseBody)
        _ -> pure Nothing
  where
    voucherCode = genVoucherCode pretixConfig githubId
    payload time =
        object
            [ "code" .= voucherCode
            , "max_usages" .= (1 :: Int)
            , "price_mode" .= ("percent" :: Text)
            , "value" .= (100 :: Int)
            , "item" .= voucherItem
            , "tag" .= ("contributor" :: Text)
            , "comment"
                .= Text.unlines
                    [ Text.unwords
                        [ "Contributor"
                        , githubUsername
                        , "(" <> ishow githubId <> ")"
                        ]
                    , Text.unwords ["Generated on", fromString $ iso8601Show time]
                    ]
            ]
    url =
        fromText . Text.intercalate "/" $
            [ "https://pretix.eu/api/v1/organizers"
            , organizer
            , "events"
            , event
            , "vouchers"
            , ""
            ]
    opts = Wreq.defaults & Wreq.auth ?~ Wreq.oauth2Token (Text.encodeUtf8 apiToken)

getExistingVoucher :: (Wreq :> es) => PretixConfig -> Int -> Eff es (Maybe Voucher)
getExistingVoucher PretixConfig{..} id' =
    Wreq.getWith opts url >>= \case
        response
            | response ^. Wreq.responseStatus == status200 -> pure $ Aeson.decode (response ^. Wreq.responseBody)
        _ -> pure Nothing
  where
    url =
        fromText . Text.intercalate "/" $
            [ "https://pretix.eu/api/v1/organizers"
            , organizer
            , "events"
            , event
            , "vouchers"
            , ishow id'
            , ""
            ]
    opts = Wreq.defaults & Wreq.auth ?~ Wreq.oauth2Token (Text.encodeUtf8 apiToken)

newtype Results a = Results {results :: [a]}
    deriving stock (Generic)
    deriving anyclass (FromJSON)

getAllVouchers :: (Wreq :> es) => PretixConfig -> Eff es [Voucher]
getAllVouchers PretixConfig{..} =
    Wreq.getWith opts url >>= \case
        response
            | response ^. Wreq.responseStatus == status200
            , Just vouchers <- Aeson.decode @(Results Voucher) $ response ^. Wreq.responseBody ->
                pure vouchers.results
        _ -> pure []
  where
    url =
        fromText . Text.intercalate "/" $
            [ "https://pretix.eu/api/v1/organizers"
            , organizer
            , "events"
            , event
            , "vouchers"
            , ""
            ]
    opts = Wreq.defaults & Wreq.auth ?~ Wreq.oauth2Token (Text.encodeUtf8 apiToken)
