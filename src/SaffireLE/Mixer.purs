module SaffireLE.Mixer where

import SPrelude

import SaffireLE.Mixer.HiRes as H
import SaffireLE.Mixer.LowRes as L

type MixValue = Number

type OutOpts =
    { mute        :: Boolean
    , attenuation :: Int
    }


newtype MixerState
    = MixerState MixerState'

type MixerState' =
    { lowResMixer      :: L.StereoMixer
    , highResMixer     :: H.StereoMixer
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
defaultMixerState :: MixerState
defaultMixerState =
    MixerState
    { lowResMixer: L.defaultMixer
    , highResMixer: H.defaultMixer
    , in3Gain: false
    , in4Gain: false
    , out12Opts: { mute: false, attenuation: 0}
    , out34Opts: { mute: false, attenuation: 0}
    , out56Opts: { mute: false, attenuation: 0}
    , midiThru: false
    , spdifTransparent: false
    }
