module Logger
  ( debug
  , warn
  , error
  ) where

import SPrelude

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console

debug :: forall m. MonadEffect m => String -> m Unit
debug = liftEffect ∘ Console.log ∘ ("[DEBUG] " <> _)

warn :: forall m. MonadEffect m => String -> m Unit
warn = liftEffect ∘ Console.warn

error :: forall m. MonadEffect m => String -> m Unit
error = liftEffect ∘ Console.error
