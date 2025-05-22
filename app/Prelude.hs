module Prelude
    ( module Prelude
    , module Control.Applicative
    , module Control.Lens
    , module Control.Monad
    , module Control.Monad.Error.Class
    , module Control.Monad.Extra
    , module Data.Aeson
    , module Data.ByteString
    , module Data.ByteString.Lazy
    , module Data.Function
    , module Data.Functor
    , module Data.Functor.Identity
    , module Data.Int
    , module Data.Maybe
    , module Data.String
    , module Data.Text
    , module Effectful
    , module Effectful.Error.Static
    , module Effectful.Exception
    , module Effectful.Log
    , module Effectful.Reader.Static
    , module Effectful.State.Static.Shared
    , module Effectful.Time
    , module Effectful.Wreq
    , module GHC.Generics
    )
where

import Control.Applicative ((<|>))
import Control.Lens ((.~), (?~), (^.))
import Control.Monad
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Extra
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), fromJSON, genericParseJSON, genericToJSON)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (LazyByteString)
import Data.Function ((&))
import Data.Functor
import Data.Functor.Identity
import Data.Int
import Data.Maybe
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as Text
import Effectful
import Effectful.Error.Static
import Effectful.Exception
import Effectful.Log
import Effectful.Reader.Static
import Effectful.State.Static.Shared
import Effectful.Time
import Effectful.Wreq (Wreq)
import GHC.Generics (Generic)
import "base" Prelude hiding (unzip)

infixl 4 <$$>

(<$$>) :: (Functor f1) => (Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) = fmap . fmap

infixl 1 <&&>

(<&&>) :: (Functor f1) => (Functor f2) => f1 (f2 a) -> (a -> b) -> f1 (f2 b)
(<&&>) = flip (<$$>)

ishow :: (Show a, IsString s) => a -> s
ishow = fromString . show

fromText :: (IsString s) => Text -> s
fromText = fromString . Text.unpack
