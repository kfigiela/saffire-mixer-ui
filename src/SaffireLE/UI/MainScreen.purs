module SaffireLE.UI.MainScreen where

import SPrelude

import Data.Int (toNumber)
import Data.Number.Format (fixed, toStringWith)
import MDC.Widgets (slider, switch, toggle, toggle')
import Math (log)
import SaffireLE.Backend (Backend)
import SaffireLE.Mixer (MixerState)
import SaffireLE.Mixer.HiRes as H
import SaffireLE.Mixer.LowRes (ChannelMix, adjustMonoChannel1Mix, adjustMonoChannel2Mix, adjustStereoChannelMix, stereoMixValueToMono, stereoMixValueToStereo, toggleChannelMix)
import SaffireLE.Mixer.LowRes as L
import SaffireLE.Status (AudioStatus(..), VUMeters)
import SaffireLE.UI.VUMeter (vuMeter)
import Specular.Dom.Element (attr, attrD, attrWhenD, onClick_)
import Specular.Dom.Widget (Widget, emptyWidget)
import Specular.FRP (unlessD, whenD, whenJustD)
import Specular.Ref (Ref(..), pureFocusRef)

header :: Widget Unit
header = do
  el "header" [class_ "mdc-top-app-bar", class_ "mdc-top-app-bar--dense", class_ "header"] do
    el "div" [class_ "mdc-top-app-bar__row"] do
      el "section" [class_ "mdc-top-app-bar__section", class_ "mdc-top-app-bar__section--align-start"] do
        el "button" [class_ "mdc-icon-button", class_ "material-icons", class_ "mdc-top-app-bar__navigation-icon--unbounded"] do text "menu"
        el "span" [class_ "mdc-top-app-bar__title"] do text "Saffire LE Mixer"
      el "section" [class_ "mdc-top-app-bar__section", class_ "mdc-top-app-bar__section--align-end"] do
        el "button" [class_ "mdc-icon-button", class_ "material-icons", class_ "mdc-top-app-bar__action-item--unbounded"] do text "file_download"
        el "button" [class_ "mdc-icon-button", class_ "material-icons", class_ "mdc-top-app-bar__action-item--unbounded"] do text "print"
        el "button" [class_ "mdc-icon-button", class_ "material-icons", class_ "mdc-top-app-bar__action-item--unbounded"] do text "info"

mainWidget :: Backend -> Widget Unit
mainWidget {meters, state, updateState, persistState} = do
  displayHighResRef <- newRef false
  let
    whenHD = whenD (refValue displayHighResRef)
    whenSD = unlessD (refValue displayHighResRef)

  header

  el "div" [class_ "container"] do
    channelGroup do
      whenSD $ mixGroupLowResControls stereoIn12 $ lowResMixer stateRef
      whenHD $ mixGroupHiResRecMixControls in1 in2 $ recMix $ highResMixer stateRef
      mixGroupMeters
        { name1: "In 1", name2: "In 2"
        , meter1: _.in1, meter2: _.in2
        }
    channelGroup do
      whenSD $ mixGroupLowResControls stereoIn34 $ lowResMixer stateRef
      whenHD $ mixGroupHiResRecMixControls in3 in4 $ recMix $ highResMixer stateRef
      mixGroupMeters
        { name1: "In 3", name2: "In 4"
        , meter1: _.in3, meter2: _.in4
        }
    channelGroup do
      whenSD $ mixGroupLowResControls stereoSpdif12 $ lowResMixer stateRef
      whenHD $ mixGroupHiResRecMixControls spdif1 spdif2 $ recMix $ highResMixer stateRef
      mixGroupMeters
        { name1: "SPDIF 1" , name2: "SPDIF 2"
        , meter1: _.spdifIn1, meter2: _.spdifIn2
        }
    divider

    channelGroup do
      whenSD $ mixGroupLowResControls stereoDAC12 $ lowResMixer stateRef
      whenHD $ mixGroupHiResOutMixControls dac12 $ highResMixer stateRef
      mixGroupMeters
        { name1: "DAC 1" , name2: "DAC 2"
        , meter1: _.dac1, meter2: _.dac2
        }
    channelGroup do
      whenSD $ mixGroupLowResControls stereoDAC34 $ lowResMixer stateRef
      whenHD $ mixGroupHiResOutMixControls dac34 $ highResMixer stateRef
      mixGroupMeters
        { name1: "DAC 3" , name2: "DAC 4"
        , meter1: _.dac3, meter2: _.dac4
        }
    channelGroup do
      whenSD $ mixGroupLowResControls stereoDAC56 $ lowResMixer stateRef
      whenHD $ mixGroupHiResOutMixControls recMix $ highResMixer stateRef
      mixGroupMeters
        { name1: "DAC 5 / Out 5" , name2: "DAC 6 / Out 5"
        , meter1: _.out5, meter2: _.out6
        }
    channelGroup do
      whenSD $ mixGroupLowResControls stereoDAC78 $ lowResMixer stateRef
      whenHD $ mixGroupNullControls
      mixGroupMeters
        { name1: "DAC 7 / SPDIF 1" , name2: "DAC 8 / SPDIF 2"
        , meter1: _.spdifOut7, meter2: _.spdifOut8
        }
    divider

    channelGroup do
      el "div" [class_ "mdc-layout-grid__cell"] do
        vuMeter "Out 1" $ _.out1 ∘ _.meters <$> meters
        vuMeter "Out 2" $ _.out2 ∘ _.meters <$> meters
      el "div" [class_ "mdc-layout-grid__cell"] do
        vuMeter "Out 3" $ _.out3 ∘ _.meters <$> meters
        vuMeter "Out 4" $ _.out4 ∘ _.meters <$> meters
      el "div" [class_ "mdc-layout-grid__cell"] do
        vuMeter "Out 5" $ _.out5 ∘ _.meters <$> meters
        vuMeter "Out 6" $ _.out6 ∘ _.meters <$> meters

    divider

    channelGroup do
      el "div" [class_ "mdc-layout-grid__cell"] do
        el "div" [class_ "outopts"] do
          el "label" [class_ "outopts__label"] do
            muteToggle $ mute $ out12Opts $ stateRef
          el "div" [class_ "outopts__control"] do
            attenuationSlider $ attenuation' $ out12Opts $ stateRef

      el "div" [class_ "mdc-layout-grid__cell"] do
        el "div" [class_ "outopts"] do
          el "label" [class_ "outopts__label"] do
            muteToggle $ mute $ out34Opts $ stateRef
          el "div" [class_ "outopts__control"] do
            attenuationSlider $ attenuation' $ out34Opts $ stateRef

      el "div" [class_ "mdc-layout-grid__cell"] do
        el "div" [class_ "outopts"] do
          el "label" [class_ "outopts__label"] do
            muteToggle $ mute $ out56Opts $ stateRef
          el "div" [class_ "outopts__control"] do
            attenuationSlider $ attenuation' $ out56Opts $ stateRef

    divider

    el "div" [class_ "option-switches"] do
      el "label" [class_ "option-switches__option", class_ "mdc-typography--caption"] do
        el "span" [class_ "material-icons"] $ dynText $ displayAudioOnIcon ∘ _.audioOn <$> meters
        dynText $ displayAudioOnLabel ∘ _.audioOn <$> meters
      el "label" [class_ "option-switches__option", class_ "mdc-typography--caption"] do
        switch displayHighResRef
        text "HD audio"
      el "label" [class_ "option-switches__option", class_ "mdc-typography--caption"] do
        switch $ spdifTransparent stateRef
        text "SPDIF transparent"
      el "label" [class_ "option-switches__option", class_ "mdc-typography--caption"] do
        switch $ midiThru stateRef
        text "MIDI Thru"
      whenSD do
        el "label" [class_ "option-switches__option", class_ "mdc-typography--caption"] do
          switch $ out12ToSpdif $ lowResMixer stateRef
          text "Out 1 & 2 to SPDIF"
      whenHD do
        el "label" [class_ "option-switches__option", class_ "mdc-typography--caption"] do
          switch $ out12ToSpdif $ highResMixer stateRef
          text "Out 1 & 2 to SPDIF"
      el "label" [class_ "option-switches__option", class_ "mdc-typography--caption"] do
        switch $ in3Gain stateRef
        text "Input 3 high gain"
      el "label" [class_ "option-switches__option", class_ "mdc-typography--caption"] do
        switch $ in4Gain stateRef
        text "Input 4 high gain"
      el "label" [class_ "option-switches__option", class_ "mdc-typography--caption"] do
        el "button" [attr "type" "button", class_ "mdc-button", onClick_ persistState, attrWhenD ((_ == NotConnected) ∘ _.audioOn <$> meters) "disabled" "disabled"] $ text "Store settings"

  where
  channelGroup :: Widget Unit -> Widget Unit
  channelGroup body =
    el "div" [class_ "mdc-layout-grid", class_ "channel-group"] do
      el "div" [class_ "mdc-layout-grid__inner"] body

  mixGroupLowResControls :: (Ref L.StereoMix -> Ref L.StereoMixValue) -> Ref L.StereoMixer -> Widget Unit
  mixGroupLowResControls lens ref = do
    el "div" [classes ["mdc-layout-grid__cell"]] do
      lowResMixControls $ lens $ stereo1 ref
    el "div" [classes ["mdc-layout-grid__cell"]] do
      lowResMixControls $ lens $ stereo2 ref

  mixGroupHiResRecMixControls :: (Ref H.RecMix -> Ref H.RecMixValue) -> (Ref H.RecMix -> Ref H.RecMixValue) -> Ref H.RecMix -> Widget Unit
  mixGroupHiResRecMixControls ch1Lens ch2Lens recMix = do
    el "div" [classes ["mdc-layout-grid__cell", "mdc-layout-grid__cell--span-2"]] do emptyWidget
    el "div" [classes ["mdc-layout-grid__cell"]] do
      el "div" [class_ "mix-control"] do
        el "div" [class_ "mix-control__controls"] do
          channelControl $ ch1Lens recMix
          channelControl $ ch2Lens recMix
    el "div" [classes ["mdc-layout-grid__cell", "mdc-layout-grid__cell--span-2"]] do emptyWidget
    where
    channelControl ref = do
      el "div" [class_ "channel-control"] do
        muteToggle $ mute' ref
        volumeSlider $ volume ref

  mixGroupHiResOutMixControls :: (Ref H.Mix -> Ref ChannelMix) -> Ref H.StereoMixer -> Widget Unit
  mixGroupHiResOutMixControls mixLens mixer = do
    el "div" [classes ["mdc-layout-grid__cell"]] do
        channelControl $ mixLens $ stereo1 mixer
    el "div" [classes ["mdc-layout-grid__cell"]] do
        channelControl $ mixLens $ stereo2 mixer
    where
    channelControl :: Ref ChannelMix -> Widget Unit
    channelControl ref = do
      el "div" [class_ "mix-control"] do
        el "div" [class_ "mix-control__controls"] do
          channelMixControls ref
  mixGroupNullControls :: Widget Unit
  mixGroupNullControls = do
    el "div" [classes ["mdc-layout-grid__cell"]] $ emptyWidget
    el "div" [classes ["mdc-layout-grid__cell"]] $ emptyWidget


  channelMixControls :: Ref ChannelMix -> Widget Unit
  channelMixControls ref = do
    el "div" [class_ "channel-control"] do
      muteToggle $ mute' ref
      volumeSlider $ volume ref
      balanceSlider $ balance ref

  mixGroupMeters ::
    { name1 :: String
    , name2 :: String
    , meter1 :: VUMeters -> Maybe Number
    , meter2 :: VUMeters -> Maybe Number
    }
    -> Widget Unit
  mixGroupMeters e = do
    el "div" [classes ["mdc-layout-grid__cell"]] do
      vuMeter e.name1 $ e.meter1 ∘ _.meters <$> meters
      vuMeter e.name2 $ e.meter2 ∘ _.meters <$> meters

  lowResMixControls :: Ref L.StereoMixValue -> Widget Unit
  lowResMixControls ref = do
    el "div" [class_ "mix-control"] do
      let isStereo = isJust ∘ stereoMixValueToStereo <$> refValue ref
      toggle' {props: [class_ "mix-control__link", attr "title" "Link as stereo channel"], on: "link", off: "link_off"} isStereo (const toggleChannelMix >$< refUpdate ref)

      el "div" [class_ "mix-control__controls"] do
        whenJustD (stereoMixValueToStereo <$> refValue ref) $ \stereo -> do
          channelMixControls $ Ref stereo (adjustStereoChannelMix >$< refUpdate ref)
        whenJustD (stereoMixValueToMono <$> refValue ref) $ \mono -> do
          channelMixControls $ Ref (_.ch1 <$> mono) (adjustMonoChannel1Mix >$< refUpdate ref)
          channelMixControls $ Ref (_.ch2 <$> mono) (adjustMonoChannel2Mix >$< refUpdate ref)

  adjustState :: Callback (MixerState -> MixerState)
  adjustState = contramapCallbackDyn ((\state' adjustment -> adjustment state') <$> state) updateState
  stateRef :: Ref MixerState
  stateRef = Ref state adjustState
  attenuationSlider ref =
    slider
     { min: 0
     , max: 0x7f
     , discrete: true
     , props: [] -- [attrD "title" (attenuationTitle <$> refValue ref)]
     , scale: [{point: 0.0, label: "-63.5 dB"}, {point: (127.0-96.0)/127.0, label: "-48"}, {point: (127.0-72.0)/127.0, label: "-36"}, {point: (127.0-48.0)/127.0, label: "-24"}, {point: (127.0-24.0)/127.0, label: "-12"}, {point: 1.0, label: "0"}]
     , currentValue: attenuationTitle
     } ref
  attenuationTitle volume = toStringWith (fixed 1) (((toNumber volume) - 127.0)*0.5) <> " dB"
  volumeSlider ref =
    slider
      { min: 0.0
      , max: 1.0
      , discrete: false
      , props: []
      , scale:
         [ {point: 0.0,  label:  "-∞ dB"}
         , {point: 0.251189, label: volumeDb 0 0.251189}
         , {point: 0.501187,  label: volumeDb 0 0.501187}
         , {point: 0.562341,  label: " "}
         , {point: 0.630957,  label: " "}
         , {point: 0.707946,  label: volumeDb 0 0.707946}
         , {point: 0.794328,  label: " "}
         , {point: 0.891251,  label: " "}
         , {point: 1.0,  label: volumeDb 0 1.0}
         ]
      , currentValue: \v -> volumeDb 2 v <> " dB"
      } ref
  volumeDb _ 0.0 = "-∞"
  volumeDb prec volume = toStringWith (fixed prec) (20.0 * (log volume/log 10.0))
  balanceSlider ref =
    slider
      { min: -1.0
      , max: 1.0
      , discrete: false
      , props: [attrD "title" (balanceTitle <$> refValue ref)]
      , scale: [{point: 0.5, label: " "}]
      , currentValue: const ""
      } ref
  balanceTitle balance = "Balance " <> toStringWith (fixed 2) balance
  muteToggle ref = toggle {props: [attr "title" "Mute"], on: "volume_mute", off: "volume_up"} ref
  divider = el "hr" [class_ "divider"] emptyWidget
  displayAudioOnLabel Idle = "Idle"
  displayAudioOnLabel Running = "In use"
  displayAudioOnLabel NotConnected = "Not connected"
  displayAudioOnIcon Idle = "pause"
  displayAudioOnIcon Running = "play_arrow"
  displayAudioOnIcon NotConnected = "error_outline"

-- "Lenses"
lowResMixer :: forall e t. Ref { lowResMixer :: t | e } -> Ref t
lowResMixer = pureFocusRef { get: _.lowResMixer, set: (\s value -> s { lowResMixer = value }) }
highResMixer :: forall e t. Ref { highResMixer :: t | e } -> Ref t
highResMixer = pureFocusRef { get: _.highResMixer, set: (\s value -> s { highResMixer = value }) }
out12Opts :: forall e t. Ref { out12Opts :: t | e } -> Ref t
out12Opts = pureFocusRef { get: _.out12Opts, set: (\s value -> s { out12Opts = value })}
out34Opts :: forall e t. Ref { out34Opts :: t | e } -> Ref t
out34Opts = pureFocusRef { get: _.out34Opts, set: (\s value -> s { out34Opts = value })}
out56Opts :: forall e t. Ref { out56Opts :: t | e } -> Ref t
out56Opts = pureFocusRef { get: _.out56Opts, set: (\s value -> s { out56Opts = value })}
volume :: forall e t. Ref { volume :: t | e } -> Ref t
volume = pureFocusRef { get: _.volume, set: (\s value -> s { volume = value })}
attenuation' :: forall e. Ref { attenuation :: Int | e } -> Ref Int
attenuation' = pureFocusRef { get: \s -> 127 - s.attenuation, set: (\s value -> s { attenuation = 127 - value })}
mute :: forall e. Ref { mute :: Boolean | e } -> Ref Boolean
mute = pureFocusRef { get: _.mute, set: (\s value -> s { mute = value })}
mute' :: forall e. Ref { enabled :: Boolean | e } -> Ref Boolean
mute' = pureFocusRef { get: not ∘ _.enabled, set: (\s value -> s { enabled = not value })}
balance :: forall e t. Ref { balance :: t | e } -> Ref t
balance = pureFocusRef { get: _.balance, set: (\s value -> s { balance = value })}
stereo1 :: forall e t. Ref { stereo1 :: t | e } -> Ref t
stereo1 = pureFocusRef { get: _.stereo1, set: (\s value -> s { stereo1 = value })}
stereo2 :: forall e t. Ref { stereo2 :: t | e } -> Ref t
stereo2 = pureFocusRef { get: _.stereo2, set: (\s value -> s { stereo2 = value })}
stereoIn12 :: forall e t. Ref { stereoIn12 :: t | e } -> Ref t
stereoIn12 = pureFocusRef { get: _.stereoIn12, set: (\s value -> s { stereoIn12 = value })}
stereoIn34 :: forall e t. Ref { stereoIn34 :: t | e } -> Ref t
stereoIn34 = pureFocusRef { get: _.stereoIn34, set: (\s value -> s { stereoIn34 = value })}
stereoSpdif12 :: forall e t. Ref { stereoSpdif12 :: t | e } -> Ref t
stereoSpdif12 = pureFocusRef { get: _.stereoSpdif12, set: (\s value -> s { stereoSpdif12 = value })}
stereoDAC12 :: forall e t. Ref { stereoDAC12 :: t | e } -> Ref t
stereoDAC12 = pureFocusRef { get: _.stereoDAC12, set: (\s value -> s { stereoDAC12 = value })}
stereoDAC34 :: forall e t. Ref { stereoDAC34 :: t | e } -> Ref t
stereoDAC34 = pureFocusRef { get: _.stereoDAC34, set: (\s value -> s { stereoDAC34 = value })}
stereoDAC56 :: forall e t. Ref { stereoDAC56 :: t | e } -> Ref t
stereoDAC56 = pureFocusRef { get: _.stereoDAC56, set: (\s value -> s { stereoDAC56 = value })}
stereoDAC78 :: forall e t. Ref { stereoDAC78 :: t | e } -> Ref t
stereoDAC78 = pureFocusRef { get: _.stereoDAC78, set: (\s value -> s { stereoDAC78 = value })}
in1 :: forall e t. Ref { in1 :: t | e } -> Ref t
in1 = pureFocusRef { get: _.in1, set: (\s value -> s { in1 = value })}
in2 :: forall e t. Ref { in2 :: t | e } -> Ref t
in2 = pureFocusRef { get: _.in2, set: (\s value -> s { in2 = value })}
in3 :: forall e t. Ref { in3 :: t | e } -> Ref t
in3 = pureFocusRef { get: _.in3, set: (\s value -> s { in3 = value })}
in4 :: forall e t. Ref { in4 :: t | e } -> Ref t
in4 = pureFocusRef { get: _.in4, set: (\s value -> s { in4 = value })}
recMix :: forall e t. Ref { recMix :: t | e } -> Ref t
recMix = pureFocusRef { get: _.recMix, set: (\s value -> s { recMix = value })}
spdif1 :: forall e t. Ref { spdif1 :: t | e } -> Ref t
spdif1 = pureFocusRef { get: _.spdif1, set: (\s value -> s { spdif1 = value })}
spdif2 :: forall e t. Ref { spdif2 :: t | e } -> Ref t
spdif2 = pureFocusRef { get: _.spdif2, set: (\s value -> s { spdif2 = value })}
dac12 :: forall e t. Ref { dac12 :: t | e } -> Ref t
dac12 = pureFocusRef { get: _.dac12, set: (\s value -> s { dac12 = value })}
dac34 :: forall e t. Ref { dac34 :: t | e } -> Ref t
dac34 = pureFocusRef { get: _.dac34, set: (\s value -> s { dac34 = value })}
spdifTransparent :: forall e t. Ref { spdifTransparent :: t | e} -> Ref t
spdifTransparent = pureFocusRef { get: _.spdifTransparent, set: (\s value -> s { spdifTransparent = value })}
midiThru :: forall e t. Ref { midiThru :: t | e} -> Ref t
midiThru = pureFocusRef { get: _.midiThru, set: (\s value -> s { midiThru = value })}
out12ToSpdif :: forall e t. Ref { out12ToSpdif :: t | e} -> Ref t
out12ToSpdif = pureFocusRef { get: _.out12ToSpdif, set: (\s value -> s { out12ToSpdif = value })}
in3Gain :: forall e t. Ref { in3Gain :: t | e} -> Ref t
in3Gain = pureFocusRef { get: _.in3Gain, set: (\s value -> s { in3Gain = value })}
in4Gain :: forall e t. Ref { in4Gain :: t | e} -> Ref t
in4Gain = pureFocusRef { get: _.in4Gain, set: (\s value -> s { in4Gain = value })}
