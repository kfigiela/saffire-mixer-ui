module SaffireLE.Server where

import SPrelude

import SaffireLE.Mixer as M
import SaffireLE.Mixer.HiRes as HM
import SaffireLE.Mixer.Matrix as MM
import SaffireLE.Mixer.Stereo as SM
import SaffireLE.Status (DeviceStatus)


data SaffireStatus =
    Meters DeviceStatus
    | CurrentState State

derive instance genericSaffireStatus :: Generic SaffireStatus _
instance showSaffireStatus :: Show SaffireStatus where
  show = genericShow
instance eqSaffireStatus :: Eq SaffireStatus where
  eq = genericEq
instance decodeSaffireStatus :: Decode SaffireStatus where
  decode = genericDecode encodingOpts


newtype State =
    State
    { mixer  :: M.MixerState
    , stereo :: SM.StereoMixer
    }

derive instance newtypeState :: Newtype State _
derive instance genericState :: Generic State _
instance showState :: Show State where
  show = genericShow
instance eqState :: Eq State where
  eq = genericEq
instance decodeState :: Decode State where
  decode = genericDecode encodingOpts

data OutputPair = Out12 | Out34 | Out56

derive instance genericOutputPair :: Generic OutputPair _
instance showOutputPair :: Show OutputPair where
  show = genericShow
instance eqOutputPair :: Eq OutputPair where
  eq = genericEq
instance decodeOutputPair :: Decode OutputPair where
  decode = genericDecodeEnum defaultGenericEnumOptions
instance encodeOutputPair :: Encode OutputPair where
  encode = genericEncodeEnum defaultGenericEnumOptions


data InputChannel = Ch3 | Ch4

derive instance genericInputChannel :: Generic InputChannel _
instance showInputChannel :: Show InputChannel where
  show = genericShow
instance eqInputChannel :: Eq InputChannel where
  eq = genericEq
instance decodeInputChannel :: Decode InputChannel where
  decode = genericDecodeEnum defaultGenericEnumOptions
instance encodeInputChannel :: Encode InputChannel where
  encode = genericEncodeEnum defaultGenericEnumOptions

data MixerCmd =
    Noop
    | Mute { output :: OutputPair, muted :: Boolean }
    | Attenuate { output :: OutputPair, db :: Number }
    | InGain { input :: InputChannel, gainOn :: Boolean }
    | MidiThru { midiThru :: Boolean }
    | SPDIFTransparent { spdifTransparent :: Boolean }
    | SetMatrixMixer { matrixMix :: MM.MatrixMixer }
    | SetStereoMixer { steroMix :: SM.StereoMixer }
    | SetHighResMixer { highResMix :: HM.Mixer }


derive instance genericMixerCmd :: Generic MixerCmd _
instance showMixerCmd :: Show MixerCmd where
  show = genericShow
instance eqMixerCmd :: Eq MixerCmd where
  eq = genericEq
instance decodeMixerCmd :: Decode MixerCmd where
  decode = genericDecode encodingOpts
instance encodeMixerCmd :: Encode MixerCmd where
  encode = genericEncode encodingOpts

--

defaultState :: State
defaultState =
    State
    { mixer: M.defaultMixer
    , stereo: SM.defaultMixer
    }
