module SaffireLE.UI.MainScreen where

import SPrelude

import Data.Int (round)
import Data.Number.Format (fixed, toStringWith)
import MDC.Widgets (slider, switch, toggle)
import SaffireLE.Backend (Backend)
import SaffireLE.Mixer (MixerState(..))
import SaffireLE.Mixer.HiRes as H
import SaffireLE.Mixer.LowRes (adjustMonoChannel1Mix, adjustMonoChannel2Mix, adjustStereoChannelMix, stereoMixValueToMono, stereoMixValueToStereo, toggleChannelMix)
import SaffireLE.Mixer.LowRes as L
import SaffireLE.Status (AudioStatus(..), VUMeters)
import SaffireLE.UI.VUMeter (vuMeter)
import Specular.Dom.Element (attr, attrD, attrWhenD, onClick_)
import Specular.Dom.Widget (Widget, emptyWidget)
import Specular.FRP (whenJustD)

mainWidget :: Backend -> Widget Unit
mainWidget {meters, state, updateState, persistState} = do
  el "div" [class_ "container"] do
    channelGroup do
      el "div" [class_ "mdc-layout-grid__cell"] do
        vuMeter "Out 1" $ _.out1 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
        vuMeter "Out 2" $ _.out2 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
        el "div" [class_ "outopts"] do
          el "label" [class_ "outopts__label"] do
            muteToggle (_.mute ∘ _.out12Opts ∘ unwrap <$> state) setOut12Mute
          el "div" [class_ "outopts__control"] do
            attenuationSlider ((127 - _) ∘ _.attenuation ∘ _.out12Opts ∘ unwrap <$> state) setOut12Volume

      el "div" [class_ "mdc-layout-grid__cell"] do
        vuMeter "Out 3" $ _.out3 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
        vuMeter "Out 4" $ _.out4 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
        el "div" [class_ "outopts"] do
          el "label" [class_ "outopts__label"] do
            muteToggle (_.mute ∘ _.out34Opts ∘ unwrap <$> state) setOut34Mute
          el "div" [class_ "outopts__control"] do
            attenuationSlider ((127 - _) ∘_.attenuation ∘ _.out34Opts ∘ unwrap <$> state) setOut34Volume
      el "div" [class_ "mdc-layout-grid__cell"] do
        vuMeter "Out 5" $ _.out5 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
        vuMeter "Out 6" $ _.out6 ∘ unwrap ∘ _.meters ∘ unwrap <$> meters
        el "div" [class_ "outopts"] do
          el "label" [class_ "outopts__label"] do
            muteToggle (_.mute ∘ _.out56Opts ∘ unwrap <$> state) setOut56Mute
          el "div" [class_ "outopts__control"] do
            attenuationSlider ((127 - _) ∘_.attenuation ∘ _.out56Opts ∘ unwrap <$> state) setOut56Volume

    divider

    channelGroup do
      mixGroupLowResControls
        { get: _.stereoIn12
        , modify: (\adj mix -> mix { stereoIn12 = adj mix.stereoIn12 })
        }
      -- mixGroupHiResRecMixControls
      --   { ch1:
      --     { get: _.in1
      --     , modify: (\adj mix -> mix { in1 = adj mix.in1 })
      --     }
      --   , ch2:
      --     { get: _.in2
      --     , modify: (\adj mix -> mix { in2 = adj mix.in2 })
      --     }
      --   }
      mixGroupMeters
        { name1: "In 1", name2: "In 2"
        , meter1: _.in1 ∘ unwrap, meter2: _.in2 ∘ unwrap
        }
    channelGroup do
      mixGroupLowResControls
        { get: _.stereoIn34
        , modify: (\adj mix -> mix { stereoIn34 = adj mix.stereoIn34 })
        }
      -- mixGroupHiResRecMixControls
      --   { ch1:
      --     { get: _.in3
      --     , modify: (\adj mix -> mix { in3 = adj mix.in3 })
      --     }
      --   , ch2:
      --     { get: _.in4
      --     , modify: (\adj mix -> mix { in4 = adj mix.in4 })
      --     }
      --   }
      mixGroupMeters
        { name1: "In 3", name2: "In 4"
        , meter1: _.in3 ∘ unwrap, meter2: _.in4 ∘ unwrap
        }
    channelGroup do
      mixGroupLowResControls
        { get: _.stereoSpdif12
        , modify: (\adj mix -> mix { stereoSpdif12 = adj mix.stereoSpdif12 })
        }
      -- mixGroupHiResRecMixControls
      --   { ch1:
      --     { get: _.spdif1
      --     , modify: (\adj mix -> mix { spdif1 = adj mix.spdif1 })
      --     }
      --   , ch2:
      --     { get: _.spdif2
      --     , modify: (\adj mix -> mix { spdif2 = adj mix.spdif2 })
      --     }
      --   }
      mixGroupMeters
        { name1: "SPDIF 1" , name2: "SPDIF 2"
        , meter1: _.spdifIn1 ∘ unwrap, meter2: _.spdifIn2 ∘ unwrap
        }
    divider

    channelGroup do
      mixGroupLowResControls
        { get: _.stereoDAC12
        , modify: (\adj mix -> mix { stereoDAC12 = adj mix.stereoDAC12 })
        }
      mixGroupMeters
        { name1: "DAC 1" , name2: "DAC 2"
        , meter1: _.dac1 ∘ unwrap, meter2: _.dac2 ∘ unwrap
        }
    channelGroup do
      mixGroupLowResControls
        { get: _.stereoDAC34
        , modify: (\adj mix -> mix { stereoDAC34 = adj mix.stereoDAC34 })
        }
      mixGroupMeters
        { name1: "DAC 3" , name2: "DAC 4"
        , meter1: _.dac3 ∘ unwrap, meter2: _.dac4 ∘ unwrap
        }
    channelGroup do
      mixGroupLowResControls
        { get: _.stereoDAC56
        , modify: (\adj mix -> mix { stereoDAC56 = adj mix.stereoDAC56 })
        }
      mixGroupMeters
        { name1: "DAC 5 / Out 5" , name2: "DAC 6 / Out 5"
        , meter1: _.out5 ∘ unwrap, meter2: _.out6 ∘ unwrap
        }
    channelGroup do
      mixGroupLowResControls
        { get: _.stereoDAC78
        , modify: (\adj mix -> mix { stereoDAC78 = adj mix.stereoDAC78 })
        }
      mixGroupMeters
        { name1: "DAC 7 / SPDIF 1" , name2: "DAC 8 / SPDIF 2"
        , meter1: _.spdifOut7 ∘ unwrap, meter2: _.spdifOut8 ∘ unwrap
        }
    divider

    el "div" [class_ "option-switches"] do
        el "label" [class_ "option-switches__option", class_ "mdc-typography--caption"] do
          el "span" [class_ "material-icons"] $ dynText $ displayAudioOnIcon ∘ _.audioOn ∘ unwrap <$> meters
          dynText $ displayAudioOnLabel ∘ _.audioOn ∘ unwrap <$> meters
        el "label" [class_ "option-switches__option", class_ "mdc-typography--caption"] do
          switch (pure false) (_.spdifTransparent ∘ unwrap <$> state) $ (\value -> _ { spdifTransparent = value }) >$< adjustState
          text "SPDIF transparent"
        el "label" [class_ "option-switches__option", class_ "mdc-typography--caption"] do
          switch (pure false) (_.midiThru ∘ unwrap <$> state) $ (\value -> _ { midiThru = value }) >$< adjustState
          text "MIDI Thru"
        el "label" [class_ "option-switches__option", class_ "mdc-typography--caption"] do
          switch (pure false) (_.out12ToSpdif ∘ _.lowResMixer ∘ unwrap <$> state) $ (\value -> _ { lowResMixer { out12ToSpdif = value }, highResMixer { out12ToSpdif = value } }) >$< adjustState
          text "Out 1 & 2 to SPDIF"
        el "label" [class_ "option-switches__option", class_ "mdc-typography--caption"] do
          switch (pure false) (_.in3Gain ∘ unwrap <$> state) $ (\value -> _ { in3Gain = value }) >$< adjustState
          text "Input 3 high gain"
        el "label" [class_ "option-switches__option", class_ "mdc-typography--caption"] do
          switch (pure false) (_.in4Gain ∘ unwrap <$> state) $ (\value -> _ { in4Gain = value }) >$< adjustState
          text "Input 4 high gain"
        el "label" [class_ "option-switches__option", class_ "mdc-typography--caption"] do
          el "button" [attr "type" "button", class_ "mdc-button", onClick_ persistState, attrWhenD ((_ == NotConnected) ∘ _.audioOn ∘ unwrap <$> meters) "disabled" "disabled"] $ text "Store settings"
  where
  channelGroup :: Widget Unit -> Widget Unit
  channelGroup body =
    el "div" [class_ "mdc-layout-grid", class_ "channel-group"] do
      el "div" [class_ "mdc-layout-grid__inner"] body

  mixGroupLowResControls ::
    { get :: L.StereoMix -> L.StereoMixValue
    , modify :: (L.StereoMixValue -> L.StereoMixValue) -> L.StereoMix -> L.StereoMix
    }
    -> Widget Unit
  mixGroupLowResControls e = do
    el "div" [classes ["mdc-layout-grid__cell"]] do
      lowResMixControls (e.get ∘ _.stereo1) $ \adj mixer -> mixer { stereo1 = e.modify adj mixer.stereo1 }
    el "div" [classes ["mdc-layout-grid__cell"]] do
      lowResMixControls (e.get ∘ _.stereo2) $ \adj mixer -> mixer { stereo2 = e.modify adj mixer.stereo2 }

  -- mixGroupHiResRecMixControls ::
  --   { ch1 ::
  --     { get :: H.RecMix -> H.RecMixValue
  --     , modify :: (H.RecMixValue -> H.RecMixValue) -> H.RecMix -> H.RecMix
  --     }
  --   , ch2 ::
  --     { get :: H.RecMix -> H.RecMixValue
  --     , modify :: (H.RecMixValue -> H.RecMixValue) -> H.RecMix -> H.RecMix
  --     }
  --   }
  --   -> Widget Unit
  -- mixGroupHiResRecMixControls {ch1, ch2} = do
  --   el "div" [classes ["mdc-layout-grid__cell"]] do
  --     el "div" [class_ "mix-control"] do
  --       el "div" [class_ "mix-control__controls"] do
  --         channelControl ch1
  --         channelControl ch2
  --   el "div" [classes ["mdc-layout-grid__cell"]] do emptyWidget
  --   where
  --   channelControl { get, modify } = do
  --     let recMixValue = get ∘ _.recMix ∘ _.highResMixer ∘ unwrap <$> state
  --         modify' = modify >$< (\adjustment mix -> mix { recMix = adjustment mix.recMix }) >$< adjustHiResMixer
  --     el "div" [class_ "channel-control"] do
  --       muteToggle (not ∘ _.enabled <$> recMixValue) $ (\muted   -> (_ { enabled = not muted })) >$< modify'
  --       volumeSlider (_.volume <$> recMixValue)      $ (\volume  -> (_ { volume = volume })) >$< modify'

  mixGroupMeters ::
    { name1 :: String
    , name2 :: String
    , meter1 :: VUMeters -> Maybe Number
    , meter2 :: VUMeters -> Maybe Number
    }
    -> Widget Unit
  mixGroupMeters e = do
    el "div" [classes ["mdc-layout-grid__cell"]] do
      vuMeter e.name1 $ e.meter1 ∘ _.meters ∘ unwrap <$> meters
      vuMeter e.name2 $ e.meter2 ∘ _.meters ∘ unwrap <$> meters

  lowResMixControls :: (L.StereoMixer -> L.StereoMixValue) -> ((L.StereoMixValue -> L.StereoMixValue) -> L.StereoMixer -> L.StereoMixer) -> Widget Unit
  lowResMixControls get modify = do
    el "div" [class_ "mix-control"] do
      let
        modify' :: Callback (L.StereoMixValue -> L.StereoMixValue)
        modify' = modify >$< adjustLowResMixer
      let mix = get ∘ _.lowResMixer ∘ unwrap <$> state
      let isStereo = isJust ∘ stereoMixValueToStereo <$> mix
      toggle {props: [class_ "mix-control__link", attr "title" "Link as stereo channel"], on: "link", off: "link_off"} isStereo (const toggleChannelMix >$< modify')

      el "div" [class_ "mix-control__controls"] do
        whenJustD (stereoMixValueToStereo <$> mix) $ \stereo -> do
          el "div" [class_ "channel-control"] do
            muteToggle (not ∘ _.enabled <$> stereo) $ (\muted   -> adjustStereoChannelMix (_ { enabled = not muted })) >$< modify'
            volumeSlider (_.volume <$> stereo)   $ (\volume  -> adjustStereoChannelMix (_ { volume = volume })) >$< modify'
            balanceSlider (_.balance <$> stereo) $ (\balance -> adjustStereoChannelMix (_ { balance = balance })) >$< modify'
        whenJustD (stereoMixValueToMono <$> mix) $ \mono -> do
          el "div" [class_ "channel-control"] do
            muteToggle (not ∘ _.enabled ∘ _.ch1 <$> mono) $ (\muted   -> adjustMonoChannel1Mix (_ { enabled = not muted })) >$< modify'
            volumeSlider (_.volume ∘ _.ch1 <$> mono)   $ (\volume  -> adjustMonoChannel1Mix (_ { volume  = volume })) >$< modify'
            balanceSlider (_.balance ∘ _.ch1 <$> mono) $ (\balance -> adjustMonoChannel1Mix (_ { balance = balance })) >$< modify'
          el "div" [class_ "channel-control"] do
            muteToggle (not ∘ _.enabled ∘ _.ch2 <$> mono) $ (\muted   -> adjustMonoChannel2Mix (_ { enabled = not muted })) >$< modify'
            volumeSlider (_.volume ∘ _.ch2 <$> mono)   $ (\volume  -> adjustMonoChannel2Mix (_ { volume  = volume })) >$< modify'
            balanceSlider (_.balance ∘ _.ch2 <$> mono) $ (\balance -> adjustMonoChannel2Mix (_ { balance = balance })) >$< modify'

  adjustState = contramapCallbackDyn ((\state' adjustment -> wrap (adjustment (unwrap state'))) <$> state) updateState
  adjustLowResMixer :: Callback (L.StereoMixer -> L.StereoMixer)
  adjustLowResMixer = (\adjustment state' -> state' { lowResMixer = adjustment state'.lowResMixer} ) >$< adjustState
  adjustHiResMixer :: Callback (H.StereoMixer -> H.StereoMixer)
  adjustHiResMixer = (\adjustment state' -> state' { highResMixer = adjustment state'.highResMixer} ) >$< adjustState
  attenuationSlider value = slider {min: 0, max: 0x7f, discrete: true, props: [attrD "title" (attenuationTitle <$> value)]} value
  attenuationTitle volume = "Volume " <> show volume
  volumeSlider  value = slider {min:  0.0, max: 1.0, discrete: false, props: [attrD "title" (volumeTitle <$> value)]} value
  volumeTitle volume = "Volume " <> show (round $ volume * 100.0) <> "%"
  balanceSlider value = slider {min: -1.0, max: 1.0, discrete: false, props: [attrD "title" (balanceTitle <$> value)]} value
  balanceTitle balance = "Balance " <> toStringWith (fixed 2) balance
  muteToggle = toggle {props: [attr "title" "Mute"], on: "volume_mute", off: "volume_up"}
  divider = el "hr" [class_ "divider"] emptyWidget
  displayAudioOnLabel Idle = "Idle"
  displayAudioOnLabel Running = "In use"
  displayAudioOnLabel NotConnected = "Not connected"
  displayAudioOnIcon Idle = "pause"
  displayAudioOnIcon Running = "play_arrow"
  displayAudioOnIcon NotConnected = "error_outline"
  setOut12Mute = (\value -> _ { out12Opts { mute = value }}) >$< adjustState
  setOut34Mute = (\value -> _ { out34Opts { mute = value }}) >$< adjustState
  setOut56Mute = (\value -> _ { out56Opts { mute = value }}) >$< adjustState
  setOut12Volume = (\value -> _ { out12Opts { attenuation = 127 - value }}) >$< adjustState
  setOut34Volume = (\value -> _ { out34Opts { attenuation = 127 - value }}) >$< adjustState
  setOut56Volume = (\value -> _ { out56Opts { attenuation = 127 - value }}) >$< adjustState

header :: Widget Unit
header = do
  el "header" [classes ["mdc-top-app-bar", "mdc-top-app-bar--fixed"], attr "style" "top: 0px;"] do
    el "div" [class_ "mdc-top-app-bar__row"] do
      el "section" [classes ["mdc-top-app-bar__section", "mdc-top-app-bar__section--align-start"]] do
        el "button" [classes ["mdc-icon-button", "material-icons", "mdc-top-app-bar__navigation-icon"]] $ text "menu"
        el "span" [class_ "mdc-top-app-bar__title"] $ text "Focusrite SaffireLE reboot"
