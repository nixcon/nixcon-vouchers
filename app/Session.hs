module Session where

import Prelude

data Session = Session
    { githubId :: Int
    , githubUsername :: Text
    }
    deriving stock (Generic)
    deriving anyclass (FromJSON, ToJSON)

newtype Tag' (a :: k) r = Tag' {unTag :: r}

type OptionalSession = Tag' "optional-session" (Maybe Session)

type RequiredSession = Tag' "required-session" Session
