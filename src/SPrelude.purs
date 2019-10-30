module SPrelude
  ( module Prelude
  , module Data.Either
  , module Data.Maybe
  , module Data.Functor.Contravariant
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
  , module Effect
  , module Effect.Class
  , module Effect.Uncurried
  , module Specular.Callback
  , module Specular.Ref
  , module Specular.Dom.Browser
  , module Specular.Dom.Element
  , module Specular.Dom.Widget
  , module Specular.FRP
  , module Data.Monoid.Extra
  ) where

import Prelude

import Control.Alternative as Control.Alternative
import Control.Semigroupoid (composeFlipped)
import Data.Either as Data.Either
import Data.Foldable as Data.Foldable
import Data.Functor.Contravariant ((>$<))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Monoid.Extra as Data.Monoid.Extra
import Data.Maybe as Data.Maybe
import Data.Newtype (class Newtype, wrap, unwrap) as Data.Newtype
import Data.Traversable hiding (for) as Data.Traversable
import Data.Tuple as Data.Tuple
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Foreign.Generic.EnumEncoding (defaultGenericEnumOptions, genericDecodeEnum, genericEncodeEnum)
import Foreign.Generic.Types (Options)
import Specular.Callback (Callback, attachEvent, contramapCallbackDyn, contramapCallbackDynMaybe, triggerCallback)
import Specular.Dom.Browser ((:=))
import Specular.Dom.Element (Prop, attrs, attrsD, classUnlessD, classWhenD, class_, classes, classesD, el, el', el_, rawHtml, text)
import Specular.Dom.Widget (Widget)
import Specular.FRP (Dynamic, _subscribeEvent, changed, readDynamic, subscribeDyn_)
import Specular.Ref (newRef, refUpdate, refUpdateConst, refValue)

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

type Widget_ = Widget Unit


-- contraMapDyn' :: Dynamic a -> Callback a -> Callback Unit
