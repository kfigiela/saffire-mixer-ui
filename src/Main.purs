module Main where

import SPrelude

import Control.Monad.Cleanup (onCleanup)
import Control.Monad.Except (runExcept, runExceptT)
import Data.Bifunctor (lmap)
import Data.Either (Either(Left, Right), either)
import Data.Functor (map)
import Data.Functor.Contravariant ((>$<))
import Data.List (List, singleton)
import Data.List.NonEmpty (toList)
import Data.Maybe (maybe)
import Effect (Effect)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Timer (setTimeout)
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn1, mkEffectFn2, runEffectFn1, runEffectFn2)
import Foreign (readString, renderForeignError)
import Foreign.Generic (decodeJSON, encodeJSON)
import Logger as Logger
import Prelude (Unit, bind, discard, pure, ($), (<<<), (<>), (==), (>>=))
import SaffireLE.Server (MixerCmd(..), OutputPair(..), SaffireStatus(..), defaultState)
import SaffireLE.Status (DeviceStatus(..), VUMeters(..), defaultDeviceStatus)
import Specular.Callback (Callback, attachEvent, mkCallback, triggerCallback)
import Specular.Dom.Browser (Node, (:=))
import Specular.Dom.Builder.Class (domEventWithSample, dynText, rawHtml)
import Specular.Dom.Element (Prop(..), text)
import Specular.Dom.Element as E
import Specular.Dom.Widget (Widget, runMainWidgetInBody)
import Specular.Dom.Widgets.Input (checkboxView)
import Specular.FRP (Dynamic, _subscribeEvent, changed, newDynamic, newEvent, readDynamic, subscribeDyn, subscribeDyn_, subscribeEvent_, weaken)
import Specular.Internal.Effect (pushDelayed)
import Specular.Ref (Ref, newRef, refUpdateConst, refValue)
import Web.Event.Event (Event)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window) as DOM
import Web.HTML.Location (protocol, host) as DOM
import Web.HTML.Window (location) as DOM
import Web.Socket.BinaryType (BinaryType(ArrayBuffer))
import Web.Socket.Event.EventTypes (onOpen, onMessage) as WS
import Web.Socket.Event.MessageEvent (MessageEvent, fromEvent, data_)
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.WebSocket (create, sendString, sendArrayBufferView, setBinaryType, toEventTarget) as WS
import Data.Number.Format (toStringWith, fixed)


main :: Effect Unit
main = runMainWidgetInBody mainWidget

mainWidget :: Widget Unit
mainWidget = do
    ws <- liftEffect $ WS.create "ws://localhost:3000" []

    messageE <- newEvent
    liftEffect $ onMsg ws messageE.fire (Logger.error ∘ show)
    {dynamic: meters, set: setMeters} <- newDynamic defaultDeviceStatus
    {dynamic: state, set: setState} <- newDynamic defaultState
    flip subscribeEvent_ messageE.event $ \evt -> do
        let msg :: Maybe SaffireStatus
            msg = hush $ runExcept $ decodeJSON evt
        whenJust msg $ case _ of
            Meters metersData -> setMeters metersData
            CurrentState stateData -> setState stateData
    let sendMixerCmd :: Callback MixerCmd
        sendMixerCmd = mkCallback $ \mixerCmd -> do
            WS.sendString ws $ encodeJSON mixerCmd
        attenuate :: OutputPair -> Callback Number
        attenuate pair = (\db -> Attenuate { output: pair, db }) >$< sendMixerCmd
        muteOutput :: OutputPair -> Callback Boolean
        muteOutput pair = (\muted -> Mute { output: pair, muted }) >$< sendMixerCmd


    E.el "div" [E.class_ "mdc-layout-grid"] do
        E.el "div" [E.class_ "mdc-layout-grid__inner"] do
            E.el "div" [E.class_ "mdc-layout-grid__cell"] do
                E.el "h3" [E.class_ "mdc-typography--subtitle1"] $ text "Inputs"
                vuMeter "In 1" $ _.in1 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
                vuMeter "In 2" $ _.in2 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
                vuMeter "In 3" $ _.in3 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
                vuMeter "In 4" $ _.in4 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
                vuMeter "SPDIF In 5" $ _.spdifIn1 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
                vuMeter "SPDIF In 6" $ _.spdifIn2 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
                E.el "h3" [E.class_ "mdc-typography--subtitle1"] $ text "DAC"
                vuMeter "DAC 1" $ _.dac1 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
                vuMeter "DAC 2" $ _.dac2 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
                vuMeter "DAC 3" $ _.dac3 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
                vuMeter "DAC 4" $ _.dac4 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters

    E.el "h3" [E.class_ "mdc-typography--subtitle1"] $ text "Outputs"
    E.el "div" [E.class_ "mdc-layout-grid"] do
        E.el "div" [E.class_ "mdc-layout-grid__inner"] do
            E.el "div" [E.class_ "mdc-layout-grid__cell"] do
                vuMeter "Out 1" $ _.out1 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
                vuMeter "Out 2" $ _.out2 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
            E.el "div" [E.class_ "mdc-layout-grid__cell"] do
                checkbox (_.mute ∘ unwrap ∘ _.out12Opts ∘ unwrap ∘ _.mixer ∘ unwrap <$> state) (muteOutput Out12)
                slider (_.attenuation ∘ unwrap ∘ _.out12Opts ∘ unwrap ∘ _.mixer ∘ unwrap <$> state) (attenuate Out12)
    E.el "div" [E.class_ "mdc-layout-grid"] do
        E.el "div" [E.class_ "mdc-layout-grid__inner"] do
            E.el "div" [E.class_ "mdc-layout-grid__cell"] do
                vuMeter "Out 3" $ _.out3 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
                vuMeter "Out 4" $ _.out4 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
            E.el "div" [E.class_ "mdc-layout-grid__cell"] do
                checkbox (_.mute ∘ unwrap ∘ _.out34Opts ∘ unwrap ∘ _.mixer ∘ unwrap <$> state) (muteOutput Out34)
                slider (_.attenuation ∘ unwrap ∘ _.out34Opts ∘ unwrap ∘ _.mixer ∘ unwrap <$> state) (attenuate Out34)
    E.el "div" [E.class_ "mdc-layout-grid"] do
        E.el "div" [E.class_ "mdc-layout-grid__inner"] do
            E.el "div" [E.class_ "mdc-layout-grid__cell"] do
                vuMeter "Out 5" $ _.out5 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
                vuMeter "Out 6" $ _.out6 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
            E.el "div" [E.class_ "mdc-layout-grid__cell"] do
                checkbox (_.mute ∘ unwrap ∘ _.out56Opts ∘ unwrap ∘ _.mixer ∘ unwrap <$> state) (muteOutput Out56)
                slider (_.attenuation ∘ unwrap ∘ _.out56Opts ∘ unwrap ∘ _.mixer ∘ unwrap <$> state) (attenuate Out56)
    E.el "div" [E.class_ "mdc-layout-grid"] do
        E.el "div" [E.class_ "mdc-layout-grid__inner"] do
            E.el "div" [E.class_ "mdc-layout-grid__cell"] do
                vuMeter "Spdif Out 7" $ _.spdifOut7 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
                vuMeter "Spdif Out 8" $ _.spdifOut8 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters

vuMeter :: String -> Dynamic (Maybe Number) -> Widget Unit
vuMeter chName value = do
    E.el "div" [E.class_ "vu"] do
        E.el "h3" [E.classes ["vu__label", "mdc-typography--caption"]] $ text chName
        E.el "div" [E.class_ "mdc-linear-progress", E.attrs ("role":="progressbar")] do
            E.el_ "div" [E.class_ "mdc-linear-progress__buffer"]
            E.el "div" [E.classes ["mdc-linear-progress__bar", "mdc-linear-progress__secondary-bar", "vu__peak"], transform scaleFraction] do
                E.el_ "span" [E.class_ "mdc-linear-progress__bar-inner"]
            E.el "div" [E.classes ["mdc-linear-progress__bar", "mdc-linear-progress__primary-bar", "vu__average"], transform scaleFraction] do
                E.el_ "span" [E.class_ "mdc-linear-progress__bar-inner"]

    where
        scaleFraction = dbToFraction <$> value
        dbToFraction = maybe 0.0 $ \db -> ((db + meterRange)/meterRange)
        meterRange = 100.0

slider :: Dynamic Number -> Callback Number -> Widget Unit
slider value onChange = do
    Tuple node _ <- E.el' "div" [E.class_ "mdc-slider", E.class_ "mdc-slider--discrete", E.attrs ("tabindex":="0" <> "role":="slider" <> "aria-valuemin":="0" <> "aria-valuemax":="127")] do
        E.el "div" [E.class_ "mdc-slider__track-container"] do
            E.el_ "div" [E.class_ "mdc-slider__track"]
        E.el "div" [E.class_ "mdc-slider__thumb-container"] do
            E.el "div" [E.class_ "mdc-slider__pin"] do
                E.el_ "div" [E.class_ "mdc-slider__pin-value-marker"]
            rawHtml $ """<svg class="mdc-slider__thumb" width="21" height="21"><circle cx="10.5" cy="10.5" r="7.875"></circle></svg>"""
            E.el_ "div" [E.class_ "mdc-slider__focus_ring"]
        E.el "div" [E.class_ "mdc-typography--caption"] $ dynText $ weaken $ showDb <$> value
    liftEffect $ nextTick $ do
        slider <- liftEffect $ runEffectFn1 _initSlider node
        liftEffect $ runEffectFn2 _subscribeSlider slider (triggerCallback onChange)
        initialValue <- readDynamic value
        runEffectFn2 _setSliderValue slider initialValue
        unsub <- runEffectFn2 _subscribeEvent (\val -> runEffectFn2 _setSliderValue slider val) (changed value)
        pure unit
        -- onCleanup unsub

checkbox :: Dynamic Boolean -> Callback Boolean -> Widget Unit
checkbox value onChange = do
    E.el "div" [E.class_ "mdc-form-field"] do
        E.el "div" [E.class_ "mdc-checkbox"] do
            Tuple node _ <-  E.el' "input" [E.attrs ("type":="checkbox"), E.class_ "mdc-checkbox__native-control"] (pure unit)
            subscribeDyn_ (_setCheckboxChecked node) value
            changedE <- domEventWithSample (\_ -> _getCheckboxChecked node) "change" node
            attachEvent changedE onChange
            E.el "div" [E.class_ "mdc-checkbox__background"] do
              rawHtml """<svg class="mdc-checkbox__checkmark" viewBox="0 0 24 24"><path class="mdc-checkbox__checkmark-path" fill="none" d="M1.73,12.91 8.1,19.28 22.79,4.59"/></svg>"""
              E.el_ "div" [E.class_ "mdc-checkbox__mixedmark"]
        E.el "label" [] $ text "Mute"

nextTick :: Effect Unit -> Effect Unit
nextTick = void ∘ setTimeout 0

showDb :: Number -> String
showDb value = toStringWith (fixed 1) value <> " dB"

foreign import data Slider :: Type
foreign import _setScaleX :: EffectFn1 Node (EffectFn1 Number Unit)
foreign import _initSlider :: EffectFn1 Node Slider
foreign import _subscribeSlider :: EffectFn2 Slider (Number -> Effect Unit) Unit
foreign import _setSliderValue :: EffectFn2 Slider Number Unit

foreign import _getCheckboxChecked :: Node -> Effect Boolean
foreign import _setCheckboxChecked :: Node -> Boolean -> Effect Unit



transform :: Dynamic Number -> Prop
transform valueD = Prop $ mkEffectFn2 \node cleanups -> do
    setScaleX <- runEffectFn1 _setScaleX node
    unsub <- runEffectFn2 _subscribeEvent (runEffectFn1 setScaleX) (changed valueD)
    pushDelayed cleanups unsub
    initialClasses <- readDynamic valueD
    runEffectFn1 setScaleX initialClasses

onMsg :: WebSocket -> (String -> Effect Unit) -> (List String -> Effect Unit) -> Effect Unit
onMsg ws success failure =
  let
    useCapture = false
    target = WS.toEventTarget ws
  in do
    l <- eventListener \x -> either failure success $ readEvent x
    addEventListener WS.onMessage l useCapture target
  where
    readEvent :: Event -> Either (List String) String
    readEvent e = (messageEvent e) >>= string
    string :: MessageEvent -> Either (List String) String
    string = lmap (map renderForeignError <<< toList) <<< runExcept <<< readString <<< data_
    messageEvent :: Event -> Either (List String) MessageEvent
    messageEvent = maybe (Left $ singleton "Can't get event") Right <<< fromEvent
