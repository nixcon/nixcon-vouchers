module Session where

import Prelude

data Session = Session
    { githubId :: Int
    , githubUsername :: Text
    -- ^ The reason we store the username in the session is that the user
    -- might not exist in the contributors table.
    }
    deriving stock (Generic)
    deriving anyclass (FromJSON, ToJSON)

newtype Tag' (a :: k) r = Tag' {unTag :: r}

type OptionalSession = Tag' "optional-session" (Maybe Session)

type RequiredSession = Tag' "required-session" Session
