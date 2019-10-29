module SaffireLE.Mixer where

import SPrelude

import SaffireLE.Mixer.HiRes as H
import SaffireLE.Mixer.Matrix as L

type MixValue = Number

newtype OutOpts
    = OutOpts
    { mute        :: Boolean
    , attenuation :: Int
    }

derive instance newtypeOutOpts :: Newtype OutOpts _
derive instance genericOutOpts :: Generic OutOpts _
instance showOutOpts :: Show OutOpts where
  show = genericShow
instance eqOutOpts :: Eq OutOpts where
  eq = genericEq
instance decodeOutOpts :: Decode OutOpts where
  decode = genericDecode encodingOpts
instance encodeOutOpts :: Encode OutOpts where
  encode = genericEncode encodingOpts

newtype MixerState
    = MixerState
    { lowResMixer      :: L.MatrixMixer
    , highResMixer     :: H.Mixer
    , in3Gain          :: Boolean
    , in4Gain          :: Boolean
    , out12Opts        :: OutOpts
    , out34Opts        :: OutOpts
    , out56Opts        :: OutOpts
    , midiThru         :: Boolean
    , spdifTransparent :: Boolean
    }

derive instance newtypeMixerState :: Newtype MixerState _
derive instance genericMixerState :: Generic MixerState _
instance showMixerState :: Show MixerState where
  show = genericShow
instance eqMixerState :: Eq MixerState where
  eq = genericEq
instance decodeMixerState :: Decode MixerState where
  decode = genericDecode encodingOpts
instance encodeMixerState :: Encode MixerState where
  encode = genericEncode encodingOpts
--
defaultMixer :: MixerState
defaultMixer =
    MixerState
    { lowResMixer: L.defaultMixer
    , highResMixer: H.defaultMixer
    , in3Gain: false
    , in4Gain: false
    , out12Opts: OutOpts { mute: false, attenuation: 0}
    , out34Opts: OutOpts { mute: false, attenuation: 0}
    , out56Opts: OutOpts { mute: false, attenuation: 0}
    , midiThru: false
    , spdifTransparent: false
    }
