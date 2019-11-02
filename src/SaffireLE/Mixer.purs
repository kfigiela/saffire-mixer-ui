module SaffireLE.Mixer where

import SPrelude

import SaffireLE.Mixer.HiRes as H
import SaffireLE.Mixer.LowRes as L

type MixValue = Number

type OutOpts =
    { mute        :: Boolean
    , attenuation :: Int
    }

type MixerState =
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

defaultMixerState :: MixerState
defaultMixerState =
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
