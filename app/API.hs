{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module API where

import Contributor
import OAuth2
import Servant
import Servant.HTML.Blaze (HTML)
import Servant.OAuth2 (OAuth2Routes)
import Text.Blaze.Html (Html)
import Prelude hiding ((:>))

type API = ToServantApi Routes

data Routes route = Routes
    { authGithub :: route :- AuthProtect Github :> "auth" :> "github" :> NamedRoutes (OAuth2Routes OAuth2Result)
    , anonRoutes :: route :- AuthProtect "optional-session" :> NamedRoutes AnonRoutes
    }
    deriving stock (Generic)

data AnonRoutes route = AnonRoutes
    { index :: route :- Get '[HTML] Html
    , static :: route :- "static" :> Raw
    }
    deriving stock (Generic)

data ContributorsRoutes route = ContributorsRoutes
    { getAllContributors :: route :- Get '[JSON] [Contributor]
    , putContributor :: route :- ReqBody '[JSON] Contributor :> Put '[JSON] NoContent
    , deleteContributor :: route :- Capture "githubId" Int :> Delete '[JSON] NoContent
    }
    deriving stock (Generic)

instance MimeRender PlainText LazyByteString where
    mimeRender _ = id
