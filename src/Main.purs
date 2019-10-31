module Main where

import SPrelude

import Control.Monad.Except (runExcept)
import Data.Bifunctor (lmap)
import Data.Int (floor, toNumber)
import Data.List (List, singleton)
import Data.List.NonEmpty (toList)
import Data.Number.Format (fixed, toStringWith)
import Foreign (readString, renderForeignError)
import Foreign.Generic (decodeJSON, encodeJSON)
import Foreign.NullOrUndefined (undefined)
import Logger as Logger
import MDC.Widgets (slider, switch, toggle)
import SaffireLE.Mixer (MixerState(..))
import SaffireLE.Mixer.Stereo (ChannelMix(..), StereoMix, StereoMixValue(..), StereoMixer(..))
import SaffireLE.Server (InputChannel(..), MixerCmd(..), OutputPair(..), SaffireStatus(..), State(..), defaultState)
import SaffireLE.Status (DeviceStatus(..), VUMeter, VUMeters(..), defaultDeviceStatus)
import SaffireLE.UI.VUMeter (vuMeter)
import Specular.Callback (contramapCallbackDyn_, mkCallback, nullCallback)
import Specular.Dom.Builder.Class as Specular
import Specular.Dom.Element (attr, dynText)
import Specular.Dom.Element as E
import Specular.Dom.Widget (Widget, emptyWidget, runMainWidgetInBody)
import Specular.FRP (dynamic_, newDynamic, whenJustD)
import Specular.FRP as Specular
import Web.Event.Event (Event)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML.HTMLMediaElement (volume)
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
  el "div" [class_ "container"] do
    channelGroup do
      el "div" [class_ "mdc-layout-grid__cell"] do
        vuMeter "Out 1" $ _.out1 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
        vuMeter "Out 2" $ _.out2 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
        el "div" [class_ "outopts"] do
          el "label" [class_ "outopts__label"] do
            muteToggle (_.mute ∘ unwrap ∘ _.out12Opts ∘ unwrap ∘ _.mixer ∘ unwrap <$> state) (muteOutput Out12)
          el "div" [class_ "outopts__control"] do
            slider attenuationSlider ((127 - _) ∘ _.attenuation ∘ unwrap ∘ _.out12Opts ∘ unwrap ∘ _.mixer ∘ unwrap <$> state) (attenuate Out12)

      el "div" [class_ "mdc-layout-grid__cell"] do
        vuMeter "Out 3" $ _.out3 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
        vuMeter "Out 4" $ _.out4 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
        el "div" [class_ "outopts"] do
          el "label" [class_ "outopts__label"] do
            muteToggle (_.mute ∘ unwrap ∘ _.out34Opts ∘ unwrap ∘ _.mixer ∘ unwrap <$> state) (muteOutput Out34)
          el "div" [class_ "outopts__control"] do
            slider attenuationSlider ((127 - _) ∘_.attenuation ∘ unwrap ∘ _.out34Opts ∘ unwrap ∘ _.mixer ∘ unwrap <$> state) (attenuate Out34)
      el "div" [class_ "mdc-layout-grid__cell"] do
        vuMeter "Out 5" $ _.out5 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
        vuMeter "Out 6" $ _.out6 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
        el "div" [class_ "outopts"] do
          el "label" [class_ "outopts__label"] do
            muteToggle (_.mute ∘ unwrap ∘ _.out56Opts ∘ unwrap ∘ _.mixer ∘ unwrap <$> state) (muteOutput Out56)
          el "div" [class_ "outopts__control"] do
            slider attenuationSlider ((127 - _) ∘_.attenuation ∘ unwrap ∘ _.out56Opts ∘ unwrap ∘ _.mixer ∘ unwrap <$> state) (attenuate Out56)

    el "hr" mempty emptyWidget

    channelGroup $ mixGroupControls
      { name1: "In 1", name2: "In 2"
      , meter1: _.in1 ∘ unwrap, meter2: _.in2 ∘ unwrap
      , get: _.stereoIn12
      , modify: (\adj mix -> mix { stereoIn12 = adj mix.stereoIn12 })
      }
    channelGroup $ mixGroupControls
      { name1: "In 3", name2: "In 4"
      , meter1: _.in3 ∘ unwrap, meter2: _.in4 ∘ unwrap
      , get: _.stereoIn34
      , modify: (\adj mix -> mix { stereoIn34 = adj mix.stereoIn34 })
      }
    channelGroup $ mixGroupControls
      { name1: "SPDIF 1" , name2: "SPDIF 2"
      , meter1: _.spdifIn1 ∘ unwrap, meter2: _.spdifIn2 ∘ unwrap
      , get: _.stereoSpdif12
      , modify: (\adj mix -> mix { stereoIn34 = adj mix.stereoSpdif12 })
      }

    el "hr" mempty emptyWidget

    channelGroup $ mixGroupControls
      { name1: "DAC 1" , name2: "DAC 2"
      , meter1: _.dac1 ∘ unwrap, meter2: _.dac2 ∘ unwrap
      , get: _.stereoDAC12
      , modify: (\adj mix -> mix { stereoDAC12 = adj mix.stereoDAC12 })
      }
    channelGroup $ mixGroupControls
      { name1: "DAC 3" , name2: "DAC 4"
      , meter1: _.dac3 ∘ unwrap, meter2: _.dac4 ∘ unwrap
      , get: _.stereoDAC34
      , modify: (\adj mix -> mix { stereoDAC34 = adj mix.stereoDAC34 })
      }

    channelGroup $ mixGroupControls
      { name1: "DAC 5 / Out 5" , name2: "DAC 6 / Out 5"
      , meter1: _.out5 ∘ unwrap, meter2: _.out6 ∘ unwrap
      , get: _.stereoDAC56
      , modify: (\adj mix -> mix { stereoDAC56 = adj mix.stereoDAC56 })
      }

    channelGroup $ mixGroupControls
      { name1: "DAC 7 / SPDIF 1" , name2: "DAC 8 / SPDIF 2"
      , meter1: _.spdifOut7 ∘ unwrap, meter2: _.spdifOut8 ∘ unwrap
      , get: _.stereoDAC78
      , modify: (\adj mix -> mix { stereoDAC56 = adj mix.stereoDAC78 })
      }

    el "hr" mempty emptyWidget
    el "div" [class_ "option-switches"] do
        el "label" [class_ "option-switches__option", class_ "mdc-typography--caption"] do
          switch (pure false) (_.spdifTransparent ∘ unwrap ∘ _.mixer ∘ unwrap <$> state) ((\spdifTransparent -> SPDIFTransparent { spdifTransparent }) >$< sendCommand)
          text "SPDIF transparent"
        el "label" [class_ "option-switches__option", class_ "mdc-typography--caption"] do
          switch (pure false) (_.midiThru ∘ unwrap ∘ _.mixer ∘ unwrap <$> state) ((\midiThru -> MidiThru { midiThru }) >$< sendCommand)
          text "MIDI Thru"
        el "label" [class_ "option-switches__option", class_ "mdc-typography--caption"] do
          switch (pure false) (_.out12ToSpdif ∘ _.stereo ∘ unwrap <$> state) stereoSPDIF
          text "Out 1 & 2 to SPDIF"
        el "label" [class_ "option-switches__option", class_ "mdc-typography--caption"] do
          switch (pure false) (_.in3Gain ∘ unwrap ∘ _.mixer ∘ unwrap <$> state) (inGain Ch3)
          text "Input 3 high gain"
        el "label" [class_ "option-switches__option", class_ "mdc-typography--caption"] do
          switch (pure false) (_.in4Gain ∘ unwrap ∘ _.mixer ∘ unwrap <$> state) (inGain Ch4)
          text "Input 4 high gain"
  -- matrix $ _.mixer ∘ unwrap <$> state
  where

  channelGroup body = el "div" [class_ "mdc-layout-grid", class_ "channel-group"] $ el "div" [class_ "mdc-layout-grid__inner"] body
  mixGroupControls ::
    { name1 :: String
    , name2 :: String
    , meter1 :: VUMeters -> Maybe Number
    , meter2 :: VUMeters -> Maybe Number
    , get :: StereoMix -> StereoMixValue
    , modify :: (StereoMixValue -> StereoMixValue) -> StereoMix -> StereoMix
    }
    -> Widget Unit
  mixGroupControls e = do
    el "div" [classes ["mdc-layout-grid__cell"]] do
      mixControls (e.get ∘ _.stereo1) $ \adj mixer -> mixer { stereo1 = e.modify adj mixer.stereo1 }
    el "div" [classes ["mdc-layout-grid__cell"]] do
      mixControls (e.get ∘ _.stereo2) $ \adj mixer -> mixer { stereo2 = e.modify adj mixer.stereo2 }
    el "div" [classes ["mdc-layout-grid__cell"]] do
      vuMeter e.name1 $ e.meter1 ∘ _.meters ∘ unwrap <$> meters
      vuMeter e.name2 $ e.meter2 ∘ _.meters ∘ unwrap <$> meters

  attenuate :: OutputPair -> Callback Int
  attenuate pair = (\db -> Attenuate { output: pair, db: 127 - db }) >$< sendCommand
  muteOutput :: OutputPair -> Callback Boolean
  muteOutput pair = (\muted -> Mute { output: pair, muted }) >$< sendCommand
  inGain ch = (\highGain -> InGain { input: ch, gainOn: highGain }) >$< sendCommand
  setStereoMixer = (\stereoMix -> SetStereoMixer { stereoMix }) >$< sendCommand
  adjustStereoMixer :: Callback (StereoMixer -> StereoMixer)
  adjustStereoMixer = contramapCallbackDyn ((\state adjustment -> adjustment state) ∘ _.stereo ∘ unwrap <$> state) setStereoMixer
  stereoSPDIF :: Callback Boolean
  stereoSPDIF = (\value -> _ { out12ToSpdif = value }) >$< adjustStereoMixer
  attenuationSlider = {min: 0, max: 0x7f, discrete: true, props: [attr "title" "Volume"]}
  volumeSlider = {min: 0.0, max: 1.0, discrete: false, props: [attr "title" "Volume"]}
  balanceSlider = {min: -1.0, max: 1.0, discrete: false, props: [attr "title" "Balance"]}
  muteToggle = toggle {props: [attr "title" "Mute"], on: "volume_mute", off: "volume_up"}

  mixControls :: (StereoMixer -> StereoMixValue) -> ((StereoMixValue -> StereoMixValue) -> StereoMixer -> StereoMixer) -> Widget Unit
  mixControls get modify = do
    el "div" [class_ "mix-control"] do
      let
        modify' :: Callback (StereoMixValue -> StereoMixValue)
        modify' = modify >$< adjustStereoMixer
      let mix = get ∘ _.stereo ∘ unwrap <$> state
      let isStereo = isJust ∘ stereoMixValueToStereo <$> mix
      toggle {props: [class_ "mix-control__link", attr "title" "Link as stereo channel"], on: "link", off: "link_off"} isStereo (const toggleChannelMix >$< modify')

      el "div" [class_ "mix-control__controls"] do
        whenJustD (stereoMixValueToStereo <$> mix) $ \mix' -> do
          el "div" [class_ "channel-control"] do
            muteToggle (not ∘ _.enabled <$> mix')     $ (\muted   -> adjustStereoChannelMix (_ { enabled = not muted })) >$< modify'
            slider volumeSlider (_.volume <$> mix')   $ (\volume  -> adjustStereoChannelMix (_ { volume = volume })) >$< modify'
            slider balanceSlider (_.balance <$> mix') $ (\balance -> adjustStereoChannelMix (_ { balance = balance })) >$< modify'
        whenJustD (stereoMixValueToMono <$> mix) $ \mono -> do
          el "div" [class_ "channel-control"] do
            muteToggle (not ∘ _.enabled ∘ _.ch1 <$> mono)     $ (\muted   -> adjustMonoChannel1Mix (_ { enabled = not muted })) >$< modify'
            slider volumeSlider (_.volume ∘ _.ch1 <$> mono)   $ (\volume  -> adjustMonoChannel1Mix (_ { volume  = volume })) >$< modify'
            slider balanceSlider (_.balance ∘ _.ch1 <$> mono) $ (\balance -> adjustMonoChannel1Mix (_ { balance = balance })) >$< modify'
          el "div" [class_ "channel-control"] do
            muteToggle (not ∘ _.enabled ∘ _.ch2 <$> mono)     $ (\muted   -> adjustMonoChannel2Mix (_ { enabled = not muted })) >$< modify'
            slider volumeSlider (_.volume ∘ _.ch2 <$> mono)   $ (\volume  -> adjustMonoChannel2Mix (_ { volume  = volume })) >$< modify'
            slider balanceSlider (_.balance ∘ _.ch2 <$> mono) $ (\balance -> adjustMonoChannel2Mix (_ { balance = balance })) >$< modify'



stereoMixValueToStereo :: StereoMixValue -> Maybe ChannelMix
stereoMixValueToStereo (StereoPair { ch12Volume: mix }) = Just mix
stereoMixValueToStereo _ = Nothing

stereoMixValueToMono :: StereoMixValue -> Maybe {ch1 :: ChannelMix, ch2 :: ChannelMix}
stereoMixValueToMono (MonoPair { ch1Volume: ch1, ch2Volume: ch2 }) = Just {ch1, ch2}
stereoMixValueToMono _ = Nothing

---

adjustStereoChannelMix :: (ChannelMix -> ChannelMix) -> StereoMixValue -> StereoMixValue
adjustStereoChannelMix adjustment (StereoPair { ch12Volume }) = StereoPair { ch12Volume: adjustment ch12Volume }
adjustStereoChannelMix _ mono = mono

adjustMonoChannelMix :: (ChannelMix -> ChannelMix) -> (ChannelMix -> ChannelMix) -> StereoMixValue -> StereoMixValue
adjustMonoChannelMix adjustment1 adjustment2 (MonoPair { ch1Volume, ch2Volume }) = MonoPair { ch1Volume: adjustment1 ch1Volume, ch2Volume: adjustment2 ch2Volume }
adjustMonoChannelMix _ _ stereo = stereo

adjustMonoChannel1Mix :: (ChannelMix -> ChannelMix) -> StereoMixValue -> StereoMixValue
adjustMonoChannel1Mix adjustment = adjustMonoChannelMix adjustment identity

adjustMonoChannel2Mix :: (ChannelMix -> ChannelMix) -> StereoMixValue -> StereoMixValue
adjustMonoChannel2Mix adjustment = adjustMonoChannelMix identity adjustment

toggleChannelMix :: StereoMixValue -> StereoMixValue
toggleChannelMix (MonoPair { ch1Volume, ch2Volume }) = StereoPair { ch12Volume: ch1Volume }
toggleChannelMix (StereoPair { ch12Volume }) = MonoPair { ch1Volume: ch12Volume, ch2Volume: ch12Volume }


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

--

matrix :: Dynamic MixerState -> Widget Unit
matrix state = do
  el "div" [class_ "mdc-data-table", attr "style" "width: 100%"] do
    el "table" [class_ "mdc-data-table__table"] do
      el "thead" [] do
        el "tr" [class_ "mdc-data-table__header-row"] do
          for_ inputs $ \(Tuple name _) -> el "th" headAttrs $ text name
      el "tbody" [class_ "mdc-data-table__content"] do
        mixParams "Out 1" $ _.out1 <$> lowResMatrix
        mixParams "Out 2" $ _.out2 <$> lowResMatrix
        mixParams "Out 3" $ _.out3 <$> lowResMatrix
        mixParams "Out 4" $ _.out4 <$> lowResMatrix

  where
  lowResMatrix = _.lowResMixer ∘ unwrap <$> state
  mixParams name mix = do
    el "tr" [class_ "mdc-data-table__content"] do
      for_ inputs $ \(Tuple _ getter) -> el "td" cellAttrs $ dynText $ formatNumber ∘ getter <$> mix
  formatNumber = show ∘ floor ∘ (_ * toNumber 0x7fff)
  cellAttrs = [classes ["mdc-data-table__cell", "mdc-data-table__cell--numeric"]]
  headAttrs = [classes ["mdc-data-table__header-cell", "mdc-data-table__header-cell--numeric"]]
  inputs =
    [ Tuple "DAC 1" _.dac1
    , Tuple "DAC 2" _.dac2
    , Tuple "DAC 3" _.dac3
    , Tuple "DAC 4" _.dac4
    , Tuple "DAC 5" _.dac5
    , Tuple "DAC 6" _.dac6
    , Tuple "DAC 7" _.dac7
    , Tuple "DAC 8" _.dac8
    , Tuple "In 1" _.in1
    , Tuple "In 2" _.in2
    , Tuple "In 3" _.in3
    , Tuple "In 4" _.in4
    , Tuple "SPDIF 1" _.spdif1
    , Tuple "SPDIF 2" _.spdif2
    ]

header :: Widget Unit
header = do
  el "header" [classes ["mdc-top-app-bar", "mdc-top-app-bar--fixed"], attr "style" "top: 0px;"] do
    el "div" [class_ "mdc-top-app-bar__row"] do
      el "section" [classes ["mdc-top-app-bar__section", "mdc-top-app-bar__section--align-start"]] do
        el "button" [classes ["mdc-icon-button", "material-icons", "mdc-top-app-bar__navigation-icon"]] $ text "menu"
        el "span" [class_ "mdc-top-app-bar__title"] $ text "Focusrite SaffireLE reboot"
