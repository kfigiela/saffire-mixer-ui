module Main where

import SPrelude

import SaffireLE.Backend (connectToBackend)
import SaffireLE.UI.MainScreen (mainWidget)
import Specular.Dom.Widget (emptyWidget, runMainWidgetInBody)
import Specular.FRP (whenD, whenJustD, withDynamic_)

main :: Effect Unit
main = runMainWidgetInBody do
  backendAttempt <- newRef 0
  let retryConnection = const (_ + 1) >$< refUpdate backendAttempt
  withDynamic_ (refValue backendAttempt) \attempt -> do
    backend <- connectToBackend "ws://localhost:53625/ws" retryConnection
    whenJustD backend $ flip withDynamic_ mainWidget
    whenNothingD backend do
      el "div" [class_ "spinner__container"] spinner

spinner :: Widget Unit
spinner = do
  el "div" [class_ "mdc-linear-progress", class_ "mdc-linear-progress--indeterminate", attrs ("role":="progressbar")] do
      el "div" [class_ "mdc-linear-progress__buffering-dots"] emptyWidget
      el "div" [class_ "mdc-linear-progress__buffer"] emptyWidget
      el "div" [classes ["mdc-linear-progress__bar", "mdc-linear-progress__primary-bar"]] do
          el "span" [class_ "mdc-linear-progress__bar-inner"] emptyWidget
      el "div" [classes ["mdc-linear-progress__bar", "mdc-linear-progress__secondary-bar"]] do
          el "span" [class_ "mdc-linear-progress__bar-inner"] emptyWidget

whenNothingD :: forall a. Dynamic (Maybe a) -> Widget Unit -> Widget Unit
whenNothingD dyn content = whenD (isNothing <$> dyn) content
