{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Server where

import API
import Config (PretixConfig (..))
import Contributor
import Control.Monad.Error.Class qualified as MonadError
import Data.Aeson qualified as Aeson
import Data.FileEmbed (embedDir, makeRelativeToProject)
import Data.IntMap.Strict qualified as IntMap
import Data.Text qualified as Text
import Network.Wai (Request (..))
import Network.Wai.Middleware.Auth.OAuth2 (OAuth2 (..))
import OAuth2
import Pretix (Voucher (..), createNewVoucher, getExistingVoucher)
import Servant hiding (throwError, (:>))
import Servant.OAuth2 (OAuth2Settings (..), Tag, authServer)
import Servant.OAuth2.Cookies (getSessionIdFromCookie)
import Servant.OAuth2.Hacks (getRedirectUrl)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
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
    { githubSettings :: OAuth2Settings (Eff '[Error ServerError, IOE]) Github OAuth2Result
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
                    contributor <- getContributorWithVoucher session
                    let eventUrl = Text.intercalate "/" [storeUrl, event]
                    let purchaseTicketButton = button "purchase" eventUrl
                    pure . html $ do
                        htmlHead
                        Html.body . Html.main $ do
                            Html.h1 $ "Hello " <> fromText session.githubUsername
                            case contributor of
                                Nothing -> do
                                    Html.p "You are not currently eligible for a voucher."
                                    purchaseTicketButton "Purchase a ticket"
                                Just Contributor{voucher} -> do
                                    Html.p "Thank you for your contributions to the community."
                                    forM_ voucher \Voucher{..} ->
                                        if redeemed > 0
                                            then do
                                                Html.p "Your voucher has already been redeemed."
                                                purchaseTicketButton "Purchase another ticket"
                                            else do
                                                Html.p do
                                                    Html.span "Your voucher code is: "
                                                    Html.code
                                                        ! Html.Attributes.id "voucher-code"
                                                        $ fromText code
                                                    Html.button
                                                        ! Html.Attributes.class_ "copy button"
                                                        ! Html.Attributes.onclick "navigator.clipboard.writeText(document.getElementById('voucher-code').innerText)"
                                                        $ mempty
                                                let url = Text.intercalate "/" [eventUrl, "redeem?voucher=" <> code]
                                                button "redeem" url "Redeem voucher"
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

getContributorWithVoucher
    :: ( Reader Env :> es
       , State Contributors :> es
       , Wreq :> es
       , Time :> es
       , Log :> es
       )
    => Session
    -> Eff es (Maybe Contributor)
getContributorWithVoucher session =
    get <&> IntMap.lookup session.githubId >>= mapM \contributor -> do
        Env{pretixConfig} <- ask
        voucher <-
            case contributor.voucher of
                Nothing -> createNewVoucher pretixConfig session
                Just Voucher{id} -> getExistingVoucher pretixConfig id
        let contributor' = contributor{voucher}
        modify $ IntMap.insert session.githubId contributor'
        pure contributor'
