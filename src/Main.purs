module Main where

import Prelude

import Effect (Effect)
import Specular.Dom.Element (text)
import Specular.Dom.Element as E
import Specular.Dom.Widget (runMainWidgetInBody)

main :: Effect Unit
main = do
    runMainWidgetInBody do
        E.el "div" [E.class_ "superclass"] $ text "Foo"
        E.el "div" [E.class_ "superclass"] $ text "Bar"
