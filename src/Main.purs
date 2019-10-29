module Main where

import SPrelude

import Control.Monad.Except (runExcept)
import Data.Bifunctor (lmap)
import Data.List (List, singleton)
import Data.List.NonEmpty (toList)
import Foreign (readString, renderForeignError)
import Foreign.Generic (decodeJSON, encodeJSON)
import Logger as Logger
import MDC.Widgets (slider, switch)
import SaffireLE.Mixer.Stereo (StereoMixer(..))
import SaffireLE.Server (InputChannel(..), MixerCmd(..), OutputPair(..), SaffireStatus(..), State(..), defaultState)
import SaffireLE.Status (DeviceStatus(..), defaultDeviceStatus)
import SaffireLE.UI.VUMeter (vuMeter)
import Specular.Callback (mkCallback)
import Specular.Dom.Widget (Widget, runMainWidgetInBody)
import Specular.FRP (newDynamic)
import Web.Event.Event (Event)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.Socket.Event.EventTypes as WS
import Web.Socket.Event.MessageEvent (MessageEvent, fromEvent, data_)
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.WebSocket as WS


main :: Effect Unit
main = runMainWidgetInBody do
    backend <- connectToBackend "ws://localhost:3000"
    mainWidget backend

type Backend = {meters :: Dynamic DeviceStatus, state :: Dynamic State, sendCommand :: Callback MixerCmd}

connectToBackend :: String -> Widget Backend
connectToBackend url = do
    ws <- liftEffect $ WS.create url []

    {dynamic: meters, set: setMeters} <- newDynamic defaultDeviceStatus
    {dynamic: state, set: setState} <- newDynamic defaultState

    let handleMessage evt = do
            let msg :: Maybe SaffireStatus
                msg = hush $ runExcept $ decodeJSON evt
            whenJust msg $ case _ of
                Meters metersData -> setMeters metersData
                CurrentState stateData -> setState stateData
    liftEffect $ onMsg ws handleMessage (Logger.error ∘ show)

    let sendCommand :: Callback MixerCmd
        sendCommand = mkCallback $ \mixerCmd -> do
            WS.sendString ws $ encodeJSON mixerCmd
    pure {meters, state, sendCommand}


mainWidget :: Backend -> Widget Unit
mainWidget {meters, state, sendCommand} = do
    el "h3" [class_ "mdc-typography--subtitle1"] $ text "Inputs"
    el "div" [class_ "mdc-layout-grid"] do
        el "div" [class_ "mdc-layout-grid__inner"] do
            el "div" [class_ "mdc-layout-grid__cell"] do
                vuMeter "In 1" $ _.in1 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
                vuMeter "In 2" $ _.in2 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
    el "div" [class_ "mdc-layout-grid"] do
        el "div" [class_ "mdc-layout-grid__inner"] do
            el "div" [class_ "mdc-layout-grid__cell"] do
                vuMeter "In 3" $ _.in3 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
                el "label" [class_ "mdc-typography--caption"] do
                    switch (pure false) (_.in3Gain ∘ unwrap ∘ _.mixer ∘ unwrap <$> state) (inGain Ch3)
                    text "High gain"
                vuMeter "In 4" $ _.in4 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
                el "label" [class_ "mdc-typography--caption"] do
                    switch (pure false) (_.in4Gain ∘ unwrap ∘ _.mixer ∘ unwrap <$> state) (inGain Ch4)
                    text "High gain"
    el "div" [class_ "mdc-layout-grid"] do
        el "div" [class_ "mdc-layout-grid__inner"] do
            el "div" [class_ "mdc-layout-grid__cell"] do
                vuMeter "SPDIF In 5" $ _.spdifIn1 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
                vuMeter "SPDIF In 6" $ _.spdifIn2 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
    el "h3" [class_ "mdc-typography--subtitle1"] $ text "DAC"
    el "div" [class_ "mdc-layout-grid"] do
        el "div" [class_ "mdc-layout-grid__inner"] do
            el "div" [class_ "mdc-layout-grid__cell"] do
                vuMeter "DAC 1" $ _.dac1 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
                vuMeter "DAC 2" $ _.dac2 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
    el "div" [class_ "mdc-layout-grid"] do
        el "div" [class_ "mdc-layout-grid__inner"] do
            el "div" [class_ "mdc-layout-grid__cell"] do
                vuMeter "DAC 3" $ _.dac3 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
                vuMeter "DAC 4" $ _.dac4 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters

    el "h3" [class_ "mdc-typography--subtitle1"] $ text "Outputs"
    el "div" [class_ "mdc-layout-grid"] do
        el "div" [class_ "mdc-layout-grid__inner"] do
            el "div" [class_ "mdc-layout-grid__cell"] do
                vuMeter "Out 1" $ _.out1 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
                vuMeter "Out 2" $ _.out2 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
            el "div" [class_ "mdc-layout-grid__cell"] do
                el "label" mempty do
                    switch (pure false) (not ∘ _.mute ∘ unwrap ∘ _.out12Opts ∘ unwrap ∘ _.mixer ∘ unwrap <$> state) (muteOutput Out12)
                slider attenuationSlider ((127 - _) ∘ _.attenuation ∘ unwrap ∘ _.out12Opts ∘ unwrap ∘ _.mixer ∘ unwrap <$> state) (attenuate Out12)
    el "div" [class_ "mdc-layout-grid"] do
        el "div" [class_ "mdc-layout-grid__inner"] do
            el "div" [class_ "mdc-layout-grid__cell"] do
                vuMeter "Out 3" $ _.out3 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
                vuMeter "Out 4" $ _.out4 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
            el "div" [class_ "mdc-layout-grid__cell"] do
                el "label" mempty do
                    switch (pure false) (not ∘ _.mute ∘ unwrap ∘ _.out34Opts ∘ unwrap ∘ _.mixer ∘ unwrap <$> state) (muteOutput Out34)
                slider attenuationSlider ((127 - _) ∘_.attenuation ∘ unwrap ∘ _.out34Opts ∘ unwrap ∘ _.mixer ∘ unwrap <$> state) (attenuate Out34)
    el "div" [class_ "mdc-layout-grid"] do
        el "div" [class_ "mdc-layout-grid__inner"] do
            el "div" [class_ "mdc-layout-grid__cell"] do
                vuMeter "Out 5" $ _.out5 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
                vuMeter "Out 6" $ _.out6 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
            el "div" [class_ "mdc-layout-grid__cell"] do
                el "label" mempty do
                    switch (pure false) (not ∘ _.mute ∘ unwrap ∘ _.out56Opts ∘ unwrap ∘ _.mixer ∘ unwrap <$> state) (muteOutput Out56)
                slider attenuationSlider ((127 - _) ∘_.attenuation ∘ unwrap ∘ _.out56Opts ∘ unwrap ∘ _.mixer ∘ unwrap <$> state) (attenuate Out56)
    el "div" [class_ "mdc-layout-grid"] do
        el "div" [class_ "mdc-layout-grid__inner"] do
            el "div" [class_ "mdc-layout-grid__cell"] do
                vuMeter "Spdif Out 7" $ _.spdifOut7 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
                vuMeter "Spdif Out 8" $ _.spdifOut8 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
            el "div" [class_ "mdc-layout-grid__cell"] do
                el "label" [class_ "mdc-typography--caption"] do
                    switch (pure false) (_.spdifTransparent ∘ unwrap ∘ _.mixer ∘ unwrap <$> state) ((\spdifTransparent -> SPDIFTransparent { spdifTransparent }) >$< sendCommand)
                    text "SPDIF transparent"
                el "label" [class_ "mdc-typography--caption"] do
                    switch (pure false) (_.midiThru ∘ unwrap ∘ _.mixer ∘ unwrap <$> state) ((\midiThru -> MidiThru { midiThru }) >$< sendCommand)
                    text "MIDI Thru"
                el "label" [class_ "mdc-typography--caption"] do
                    switch (pure false) (_.out12ToSpdif ∘ unwrap ∘ _.stereo ∘ unwrap <$> state) stereoSPDIF
                    text "Out 1 & 2 to SPDIF"
    where
    attenuate :: OutputPair -> Callback Int
    attenuate pair = (\db -> Attenuate { output: pair, db: 127 - db }) >$< sendCommand
    muteOutput :: OutputPair -> Callback Boolean
    muteOutput pair = (\enabled -> Mute { output: pair, muted: not enabled }) >$< sendCommand
    inGain ch = (\highGain -> InGain { input: ch, gainOn: highGain }) >$< sendCommand
    setStereoMixer = (\stereoMix -> SetStereoMixer { stereoMix }) >$< sendCommand
    adjustStereoMixer :: Callback (StereoMixer -> StereoMixer)
    adjustStereoMixer = contramapCallbackDyn ((\state adjustment -> adjustment state) ∘ _.stereo ∘ unwrap <$> state) setStereoMixer
    stereoSPDIF :: Callback Boolean
    stereoSPDIF = (\value (StereoMixer mixer) -> StereoMixer (mixer { out12ToSpdif = value })) >$< adjustStereoMixer
    attenuationSlider = {min: 0, max: 0x7f, discrete: true}

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
