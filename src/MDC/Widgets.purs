module MDC.Widgets where

import SPrelude

import Data.Number.Format (fixed, toStringWith)
import Effect.Class.Console as Logger
import Effect.Timer (setTimeout)
import Specular.Callback (contramapCallbackDyn_)
import Specular.Dom.Browser (Node)
import Specular.Dom.Builder.Class (domEventWithSample)
import Specular.Dom.Element (attr, classWhen, dynText, onClick_)
import Specular.Dom.Widget (emptyWidget)


-- Toggle / icon button


toggle :: {props :: Array Prop, on :: String, off :: String} -> Dynamic Boolean -> Callback Boolean -> Widget Unit
toggle {props, on, off} value set = do
  let toggleCb = contramapCallbackDyn_ (not <$> value) set
  iconButton ([class_ "material-icons"] <> props) toggleCb do
      dynText $ value <#> if _ then on else off

iconButton :: Array Prop -> Callback Unit -> Widget Unit -> Widget Unit
iconButton props clickCb = el "button" ([class_ "mdc-icon-button", attr "type" "button", onClick_ clickCb] <> props)

-- Slider

slider :: forall num. Show num => {min :: num, max :: num, discrete :: Boolean, props :: Array Prop} -> Dynamic num -> Callback num -> Widget Unit
slider {min, max, discrete, props} value onChange = do
    Tuple node _ <- el' "div" ([class_ "mdc-slider", attrs ("tabindex":="0" <> "role":="slider" <> "aria-valuemin":=show min <> "aria-valuemax":=show max), classWhen discrete "mdc-slider--discrete"] <> props) do
        el "div" [class_ "mdc-slider__track-container"] do
            el "div" [class_ "mdc-slider__track"] emptyWidget
        el "div" [class_ "mdc-slider__thumb-container"] do
            when discrete $ el "div" [class_ "mdc-slider__pin"] do
                el "div" [class_ "mdc-slider__pin-value-marker"] emptyWidget
            rawHtml $ """<svg class="mdc-slider__thumb" width="21" height="21"><circle cx="10.5" cy="10.5" r="7.875"></circle></svg>"""
            el "div" [class_ "mdc-slider__focus_ring"] emptyWidget
    liftEffect $ nextTick $ do
        mdcSlider <- liftEffect $ runEffectFn1 _initSlider node
        liftEffect $ runEffectFn2 _subscribeSlider mdcSlider (triggerCallback onChange)
        initialValue <- readDynamic value
        runEffectFn2 _setSliderValue mdcSlider initialValue
        unsub <- runEffectFn2 _subscribeEvent (\val -> runEffectFn2 _setSliderValue mdcSlider val) (changed value)
        pure unit
        -- onCleanup unsub

foreign import data Slider :: Type
foreign import _initSlider :: EffectFn1 Node Slider
foreign import _subscribeSlider :: forall num. EffectFn2 Slider (num -> Effect Unit) Unit
foreign import _setSliderValue :: forall num. EffectFn2 Slider num Unit

-- Checkbox

checkbox :: Dynamic Boolean -> Callback Boolean -> Widget Unit
checkbox value onChange = do
    el "div" [class_ "mdc-checkbox"] do
        rawCheckbox value onChange [class_ "mdc-checkbox__native-control"]
        el "div" [class_ "mdc-checkbox__background"] do
          rawHtml """<svg class="mdc-checkbox__checkmark" viewBox="0 0 24 24"><path class="mdc-checkbox__checkmark-path" fill="none" d="M1.73,12.91 8.1,19.28 22.79,4.59"/></svg>"""
          el "div" [class_ "mdc-checkbox__mixedmark"] emptyWidget

switch :: Dynamic Boolean -> Dynamic Boolean -> Callback Boolean -> Widget Unit
switch disabled value onChange = do
    el "div" [class_ "mdc-switch", classWhenD value "mdc-switch--checked", classWhenD disabled "mdc-switch--disabled"] do
        el "div" [class_ "mdc-switch__track"] emptyWidget
        el "div" [class_ "mdc-switch__thumb-underlay"] do
            el "div" [class_ "mdc-switch__thumb"] do
                rawCheckbox value onChange [class_ "mdc-checkbox__native-control", attrs ("role":="switch"), attrsD ((if _ then ("disabled":="disabled") else mempty) <$> disabled)]

--

rawCheckbox :: Dynamic Boolean -> Callback Boolean -> Array Prop -> Widget Unit
rawCheckbox value onChange props = do
    Tuple node _ <- el' "input" ([attrs ("type":="checkbox")] <> props) (pure unit)
    subscribeDyn_ (_setCheckboxChecked node) value
    changedE <- domEventWithSample (\_ -> Logger.info "read" *> _getCheckboxChecked node) "change" node
    attachEvent changedE onChange

foreign import _getCheckboxChecked :: Node -> Effect Boolean
foreign import _setCheckboxChecked :: Node -> Boolean -> Effect Unit

nextTick :: Effect Unit -> Effect Unit
nextTick = void ∘ setTimeout 0
