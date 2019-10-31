module SaffireLE.UI.MainScreen where

import SPrelude

import MDC.Widgets (slider, switch, toggle)
import SaffireLE.Backend (Backend)
import SaffireLE.Mixer.Stereo (StereoMix, StereoMixValue, StereoMixer, adjustMonoChannel1Mix, adjustMonoChannel2Mix, adjustStereoChannelMix, stereoMixValueToMono, stereoMixValueToStereo, toggleChannelMix)
import SaffireLE.Server (InputChannel(..), MixerCmd(..), OutputPair(..))
import SaffireLE.Status (VUMeters)
import SaffireLE.UI.VUMeter (vuMeter)
import Specular.Dom.Element (attr)
import Specular.Dom.Widget (Widget, emptyWidget)
import Specular.FRP (whenJustD)

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

    divider

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

    divider

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

    divider

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

  channelGroup body =
    el "div" [class_ "mdc-layout-grid", class_ "channel-group"] do
      el "div" [class_ "mdc-layout-grid__inner"] body
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
  adjustStereoMixer = contramapCallbackDyn ((\state' adjustment -> adjustment state') ∘ _.stereo ∘ unwrap <$> state) setStereoMixer
  stereoSPDIF :: Callback Boolean
  stereoSPDIF = (\value -> _ { out12ToSpdif = value }) >$< adjustStereoMixer
  attenuationSlider = {min: 0, max: 0x7f, discrete: true, props: [attr "title" "Volume"]}
  volumeSlider = {min: 0.0, max: 1.0, discrete: false, props: [attr "title" "Volume"]}
  balanceSlider = {min: -1.0, max: 1.0, discrete: false, props: [attr "title" "Balance"]}
  muteToggle = toggle {props: [attr "title" "Mute"], on: "volume_mute", off: "volume_up"}
  divider = el_ "hr" emptyWidget

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
        whenJustD (stereoMixValueToStereo <$> mix) $ \stereo -> do
          el "div" [class_ "channel-control"] do
            muteToggle (not ∘ _.enabled <$> stereo)     $ (\muted   -> adjustStereoChannelMix (_ { enabled = not muted })) >$< modify'
            slider volumeSlider (_.volume <$> stereo)   $ (\volume  -> adjustStereoChannelMix (_ { volume = volume })) >$< modify'
            slider balanceSlider (_.balance <$> stereo) $ (\balance -> adjustStereoChannelMix (_ { balance = balance })) >$< modify'

        whenJustD (stereoMixValueToMono <$> mix) $ \mono -> do
          el "div" [class_ "channel-control"] do
            muteToggle (not ∘ _.enabled ∘ _.ch1 <$> mono)     $ (\muted   -> adjustMonoChannel1Mix (_ { enabled = not muted })) >$< modify'
            slider volumeSlider (_.volume ∘ _.ch1 <$> mono)   $ (\volume  -> adjustMonoChannel1Mix (_ { volume  = volume })) >$< modify'
            slider balanceSlider (_.balance ∘ _.ch1 <$> mono) $ (\balance -> adjustMonoChannel1Mix (_ { balance = balance })) >$< modify'
          el "div" [class_ "channel-control"] do
            muteToggle (not ∘ _.enabled ∘ _.ch2 <$> mono)     $ (\muted   -> adjustMonoChannel2Mix (_ { enabled = not muted })) >$< modify'
            slider volumeSlider (_.volume ∘ _.ch2 <$> mono)   $ (\volume  -> adjustMonoChannel2Mix (_ { volume  = volume })) >$< modify'
            slider balanceSlider (_.balance ∘ _.ch2 <$> mono) $ (\balance -> adjustMonoChannel2Mix (_ { balance = balance })) >$< modify'

header :: Widget Unit
header = do
  el "header" [classes ["mdc-top-app-bar", "mdc-top-app-bar--fixed"], attr "style" "top: 0px;"] do
    el "div" [class_ "mdc-top-app-bar__row"] do
      el "section" [classes ["mdc-top-app-bar__section", "mdc-top-app-bar__section--align-start"]] do
        el "button" [classes ["mdc-icon-button", "material-icons", "mdc-top-app-bar__navigation-icon"]] $ text "menu"
        el "span" [class_ "mdc-top-app-bar__title"] $ text "Focusrite SaffireLE reboot"
