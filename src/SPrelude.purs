module SPrelude
  ( module Prelude
  , module Data.Either
  , module Data.Maybe
  , module Data.Traversable
  , module Data.Foldable
  , module Data.Tuple
  , module Data.Newtype
  , module Control.Alternative

  , (∘)
  , (∘∘)
  , compose2
  , (⋙)
  , module Data.Generic.Rep
  , module Data.Generic.Rep.Eq
  , module Data.Generic.Rep.Show
  , module Foreign.Class
  , module Foreign.Generic
  , module Foreign.Generic.EnumEncoding
  , encodingOpts
  , whenJust
  ) where

import Prelude

-- NB: qualified imports to avoid warnings
import Data.Either as Data.Either
import Data.Maybe as Data.Maybe
import Data.Foldable as Data.Foldable
import Data.Tuple as Data.Tuple
import Control.Alternative as Control.Alternative

import Data.Maybe (Maybe(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Foreign.Generic.EnumEncoding (defaultGenericEnumOptions, genericDecodeEnum, genericEncodeEnum)
import Foreign.Generic.Types (Options)

-- `for` conflicts with Specular.
-- The correct thing to do would be to find a better name for Specular's for,
-- but there's none yet.
import Data.Traversable hiding (for) as Data.Traversable

-- We need to hide `traverse`, as it conflicts with `Data.Traversable`.
import Data.Newtype (class Newtype, wrap, unwrap) as Data.Newtype

import Control.Semigroupoid (composeFlipped)

infixr 9 compose as ∘

infixr 9 composeFlipped as ⋙

compose2 :: forall a b c d g. Semigroupoid g => g b a -> (d -> g c b) -> d -> g c a
compose2 f g = (f ∘ _) ∘ g

infixr 9 compose2 as ∘∘

encodingOpts :: Options
encodingOpts = defaultOptions  { unwrapSingleConstructors = true, unwrapSingleRecordArguments = true }


---

whenJust :: forall m a. Applicative m => Maybe a -> (a -> m Unit) -> m Unit
whenJust (Just a) f = f a
whenJust Nothing _ = pure unit
