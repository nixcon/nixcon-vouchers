{-# OPTIONS_GHC -Wno-orphans #-}

module Log where

import Prelude

instance Enum LogLevel where
    fromEnum LogAttention = 0
    fromEnum LogInfo = 1
    fromEnum LogTrace = 2
    toEnum 0 = LogAttention
    toEnum 1 = LogInfo
    toEnum 2 = LogTrace
    toEnum _ = undefined

tryLogged :: forall es a. (Log :> es) => Text -> Text -> (Eff es) a -> (Eff es) a
tryLogged goodMessage badMessage = either logBad logGood <=< try @SomeException
  where
    logBad :: SomeException -> Eff es a
    logBad e = do
        -- logMessage LogAttention badMessage $ object ["message" .= displayException e]
        logMessage LogAttention badMessage $ object []
        throwIO e
    logGood :: a -> Eff es a
    logGood a = logMessage LogInfo goodMessage (object []) $> a
