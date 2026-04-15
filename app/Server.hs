{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Server where

import API
import Config (PretixConfig (..))
import Contributor
import Control.Monad.Error.Class qualified as MonadError
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Aeson qualified as Aeson
import Data.FileEmbed (embedDir, makeRelativeToProject)
import Data.IntMap.Strict qualified as IntMap
import Data.Ord (clamp)
import Data.Text qualified as Text
import Network.Wai (Request (..))
import Network.Wai.Middleware.Auth.OAuth2 (OAuth2 (..))
import OAuth2
import Pretix (Voucher (..), createNewVoucher, getExistingVoucher)
import Servant hiding (throwError, (:>))
import Servant.OAuth2 (OAuth2Settings (..), Tag, authServer)
import Servant.OAuth2.Cookies (getSessionIdFromCookie)
import Servant.OAuth2.Hacks (getRedirectUrl)
import Servant.Server.Experimental.Auth
    ( AuthHandler
    , AuthServerData
    , mkAuthHandler
    )
import Servant.Server.Generic
import Session
import Text.Blaze.Html5 (html, (!))
import Text.Blaze.Html5 qualified as Html
import Text.Blaze.Html5.Attributes qualified as Html.Attributes
import Web.ClientSession (Key)
import Prelude hiding (Handler)

deriving anyclass instance FromJSON Contributor

deriving anyclass instance ToJSON Contributor

data Env = Env
    { githubSettings
        :: OAuth2Settings (Eff '[Error ServerError, IOE]) Github OAuth2Result
    , pretixConfig :: PretixConfig
    }

optionalSessionHandler :: Key -> AuthHandler Request OptionalSession
optionalSessionHandler key = mkAuthHandler f
  where
    f :: Request -> Handler OptionalSession
    f req = pure . Tag' $ Aeson.decode =<< getSessionIdFromCookie req key

requiredSessionHandler :: Key -> AuthHandler Request RequiredSession
requiredSessionHandler key = mkAuthHandler f
  where
    f :: Request -> Handler RequiredSession
    f req = case Aeson.decode =<< getSessionIdFromCookie req key of
        Just sess -> pure . Tag' $ sess
        Nothing -> MonadError.throwError err403

type instance AuthServerData (AuthProtect "optional-session") = OptionalSession

type instance AuthServerData (AuthProtect "required-session") = RequiredSession

type instance AuthServerData (AuthProtect Github) = Tag Github OAuth2Result

server
    :: forall es
     . ( Reader Env :> es
       , State Contributors :> es
       , Wreq :> es
       , Time :> es
       , Log :> es
       )
    => Routes (AsServerT (Eff es))
server =
    Routes
        { authGithub = authServer
        , anonRoutes = \(session :: OptionalSession) ->
            hoistServer
                (Proxy @(NamedRoutes AnonRoutes))
                (runReader session.unTag)
                anonRoutesServer
        }

anonRoutesServer
    :: forall es
     . ( Reader (Maybe Session) :> es
       , Reader Env :> es
       , State Contributors :> es
       , Wreq :> es
       , Time :> es
       , Log :> es
       )
    => AnonRoutes (AsServerT (Eff es))
anonRoutesServer =
    AnonRoutes
        { index =
            ask @(Maybe Session) >>= \case
                Nothing -> do
                    Env{githubSettings} <- ask
                    let oauth2 = githubSettings.provider.oauth2
                    let url = getRedirectUrl githubSettings.provider.callbackUrl oauth2 oauth2.oa2Scope
                    pure . html $ do
                        htmlHead
                        Html.body . Html.main $ do
                            Html.h1 "Hello stranger"
                            Html.p "Click here to determine whether you are eligible for a voucher:"
                            button "github-login" url "Log in with GitHub"
                Just session -> do
                    Env{pretixConfig = PretixConfig{storeUrl, event}} <- ask
                    maybeContributor <- getContributor session
                    let eventUrl = Text.intercalate "/" [storeUrl, event]
                    let purchaseTicketButton = button "purchase" eventUrl
                    pure . html $ do
                        htmlHead
                        Html.body . Html.main $ do
                            Html.h1 $ "Hello " <> fromText session.githubUsername
                            case maybeContributor of
                                Just Contributor{voucher = Just Voucher{..}, ..} -> do
                                    Html.p "Thank you for your contributions to the community."
                                    Html.p "According to our data, your contributions in the past 4 years are:"
                                    Html.ul do
                                        when special . Html.li $
                                            "❄️ being a very special snowflake"
                                        when (commits > 0) . Html.li . fromString $
                                            show commits <> " merged commits in official repos"
                                        when (merges > 0) . Html.li . fromString $
                                            show merges <> " PR merges in official repos"
                                    if redeemed >= max_usages
                                        then do
                                            Html.p "Your voucher has already been redeemed."
                                            purchaseTicketButton "Purchase another ticket"
                                        else do
                                            Html.h3 . fromString $
                                                let
                                                    valueStr = Text.unpack . fromMaybe value $ Text.stripSuffix ".00" value
                                                 in
                                                    "You are eligible for a " <> valueStr <> "\xA0% voucher! 🎉"
                                            Html.p do
                                                Html.span "Your voucher code is: "
                                                Html.span
                                                    ! Html.Attributes.id "voucher-code-wrapper"
                                                    $ do
                                                        Html.code
                                                            ! Html.Attributes.id "voucher-code"
                                                            $ fromText code
                                                        Html.button
                                                            ! Html.Attributes.class_ "copy button"
                                                            ! Html.Attributes.onclick
                                                                "navigator.clipboard.writeText(document.getElementById('voucher-code').innerText)"
                                                            $ mempty
                                            let url = Text.intercalate "/" [eventUrl, "redeem?voucher=" <> code]
                                            button "redeem" url "Redeem voucher"
                                _ -> do
                                    Html.p "You are not currently eligible for a voucher."
                                    purchaseTicketButton "Purchase a ticket"
                            Html.hr
                            Html.h3 "Voucher FAQ"
                            Html.h4 "Can I give my voucher to somebody else?"
                            Html.p
                                "Yes! You can give your voucher to anyone of your choosing, regardless of whether they are a contributor, or whether you plan to buy a ticket or not."
                            Html.p "Thank you for spreading the Nix love!"
                            Html.h4 "I made some commits/merges, but the site still says I'm not eligible or not increasing my discount. What gives?"
                            Html.p "Note that we only count contributions made in the 4 years up to the month the ticket sales opened."
                            Html.p "These numbers will not be refreshed until the next NixCon, to remove the temptation of opening low-quality PRs to game the system."
                            Html.p do
                                "See the "
                                Html.a
                                    ! Html.Attributes.href "https://github.com/nixcon/nixcon-vouchers/blob/main/contributors.csv"
                                    $ "source code repository"
                                " for the exact numbers."
                            Html.h4 do
                                "Can I ask for an exception? An increase in voucher percentage? Can I get a voucher even though I am not eligible for one? A "
                                Html.em "second"
                                " voucher??"
                            Html.p do
                                "We have a system in place to handle requests for exceptions that is similar to the SC elections. Send us an email at "
                                Html.a
                                    ! Html.Attributes.href "mailto:nixcon@nixos.org"
                                    $ "nixcon@nixos.org"
                                " to explain your situation in as much detail as you can."
                            Html.p
                                "Please note that vouchers are designed to reward contributions to the Nix community. As such, we will ask you to present relevant work or volunteering efforts in the community when requesting an exception."
                            Html.p do
                                "We are very happy to make exceptions for members of the "
                                Html.a
                                    ! Html.Attributes.href "https://nixos.org/community/"
                                    $ "community teams"
                                "."
                            Html.h4
                                "My budget is tight this year. Is there any other help available other than vouchers?"
                            Html.p
                                "NixCon also runs on a tight budget. We rely on the goodwill of sponsors and supporters. As such we cannot promise too much until we have secured sponsorships and sold some tickets."
                            Html.p
                                "If the supporters and sponsors are generous and we have a budget surplus, we will be happy to offer stipends to relieve some of the burden of travel and accommodation to our attendees."
                            Html.p "Please check with us closer to the event date."
        , static = serveDirectoryEmbedded $(embedDir =<< makeRelativeToProject "static")
        }
  where
    button className href =
        Html.a
            ! Html.Attributes.class_ (className <> " button")
            ! Html.Attributes.href (fromText href)
    htmlHead =
        Html.head do
            Html.meta ! Html.Attributes.charset "utf-8"
            Html.meta
                ! Html.Attributes.name "viewport"
                ! Html.Attributes.content "width=device-width,initial-scale=1,viewport-fit=cover"
            Html.meta
                ! Html.Attributes.name "description"
                ! Html.Attributes.content "Verify your Nix contributor status"
            Html.meta
                ! Html.Attributes.property "og:image"
                ! Html.Attributes.content "https://nixos.org/images/explore/community.svg"
            Html.title "NixCon contributor verification"
            Html.style "html{visibility: hidden;opacity:0;}"
            Html.link
                ! Html.Attributes.rel "stylesheet"
                ! Html.Attributes.href "static/style.css"
            Html.link
                ! Html.Attributes.rel "icon"
                ! Html.Attributes.href "static/nix.svg"
                ! Html.Attributes.type_ "image/svg+xml"
            Html.link
                ! Html.Attributes.rel "icon"
                ! Html.Attributes.href "static/favicon.ico"

getContributor
    :: ( Reader Env :> es
       , State Contributors :> es
       , Wreq :> es
       , Time :> es
       , Log :> es
       )
    => Session
    -> Eff es (Maybe Contributor)
getContributor session =
    IntMap.lookup session.githubId <$> get >>= mapM \contributor -> do
        voucher <- runMaybeT $ do
            percent <- MaybeT . pure $
                case percentForContributor contributor of
                    p | p > 0 -> Just p
                    _ -> Nothing
            Env{pretixConfig} <- lift ask
            MaybeT $ case contributor.voucher of
                Nothing -> createNewVoucher percent pretixConfig session
                Just Voucher{id} -> getExistingVoucher pretixConfig id
        let contributor' = contributor{voucher}
        modify . setContributor $ contributor'
        pure contributor'

percentForContributor :: Contributor -> Int
percentForContributor Contributor{..}
    | special = 100
    | contributions > 9000 = 100
    | otherwise =
        clamp (0, 100)
            . (10 *)
            . ceiling @Double
            . (\c -> (logBase 16 (c ^ (64 - 16 :: Int)) + 5) / 10)
            . fromIntegral
            $ contributions
  where
    contributions, maxContributions :: Int
    contributions = sum $ min maxContributions <$> [commits, merges]
    maxContributions = 9000
