module Data.Monoid.Extra
  ( mwhen
  , munless
  , mwith
  ) where

import Prelude

import Data.Maybe (Maybe (..))

-- | Perform a monoidal action when a condition is true.
mwhen :: forall a. Monoid a => Boolean -> a -> a
mwhen false _ = mempty
mwhen true x = x

-- | Perform a monoidal action unless a condition is true.
munless :: forall a. Monoid a => Boolean -> a -> a
munless true _ = mempty
munless false x = x

-- | Elminate a Maybe into a monoidal type, returning `mempty` when the input is `Nothing`.
mwith :: forall a b. Monoid b => Maybe a -> (a -> b) -> b
mwith (Just x) f = f x
mwith Nothing  _ = mempty
