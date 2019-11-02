module Main where

import SPrelude

import SaffireLE.Backend (connectToBackend)
import SaffireLE.UI.MainScreen (mainWidget)
import Specular.Dom.Widget (runMainWidgetInBody)
import Specular.FRP (whenJustD, withDynamic_)

main :: Effect Unit
main = runMainWidgetInBody do
  backendAttempt <- newRef 0
  let retryConnection = const (_ + 1) >$< refUpdate backendAttempt
  withDynamic_ (refValue backendAttempt) \attempt -> do
    backend <- connectToBackend "ws://localhost:3000" retryConnection
    withDynamic_ backend $ case _ of
      Just backend' -> mainWidget backend'
      Nothing -> text $ "Connecting " <> show attempt
