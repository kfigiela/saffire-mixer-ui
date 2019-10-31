module Main where

import SPrelude

import SaffireLE.Backend (connectToBackend)
import SaffireLE.UI.MainScreen (mainWidget)
import Specular.Dom.Widget (runMainWidgetInBody)

main :: Effect Unit
main = runMainWidgetInBody do
  backend <- connectToBackend "ws://localhost:3000"
  mainWidget backend
