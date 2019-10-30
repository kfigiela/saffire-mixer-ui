module SaffireLE.Mixer.Stereo where

import SPrelude

type MixValue = Number

type StereoMixer =
    { stereo1      :: StereoMix
    , stereo2      :: StereoMix
    , out12ToSpdif :: Boolean
    }

type StereoMix =
    { stereoDAC12   :: StereoMixValue
    , stereoDAC34   :: StereoMixValue
    , stereoDAC56   :: StereoMixValue
    , stereoDAC78   :: StereoMixValue
    , stereoIn12    :: StereoMixValue
    , stereoIn34    :: StereoMixValue
    , stereoSpdif12 :: StereoMixValue
    , inputMix      :: Number
    , dacMix        :: Number
    }

type ChannelMix =
    { volume  :: MixValue
    , balance :: MixValue
    , enabled :: Boolean
    }

data StereoMixValue
    = StereoPair
    { ch12Volume :: ChannelMix
    }
    | MonoPair
    { ch1Volume :: ChannelMix
    , ch2Volume :: ChannelMix
    }

defEnabledStereoMix = StereoPair { ch12Volume: { volume: 1.0, balance: 0.0, enabled: true}} :: StereoMixValue
defDisabledStereoMix = StereoPair { ch12Volume: { volume: 0.0, balance: 0.0, enabled: false}} :: StereoMixValue
defDisabledMonoMix = MonoPair { ch1Volume: { volume: 0.0, balance: 0.0, enabled: false}, ch2Volume: { volume: 0.0, balance: 0.0, enabled: false}} :: StereoMixValue

derive instance genericStereoMixValue :: Generic StereoMixValue _
instance showStereoMixValue :: Show StereoMixValue where
  show = genericShow
instance eqStereoMixValue :: Eq StereoMixValue where
  eq = genericEq
instance decodeStereoMixValue :: Decode StereoMixValue where
  decode = genericDecode encodingOpts
instance encodeStereoMixValue :: Encode StereoMixValue where
  encode = genericEncode encodingOpts
--

defaultMixer :: StereoMixer
defaultMixer =
    { stereo1:
        { stereoDAC12: defEnabledStereoMix
        , stereoDAC34: defDisabledStereoMix
        , stereoDAC56: defDisabledStereoMix
        , stereoDAC78: defDisabledStereoMix
        , stereoIn12: defDisabledStereoMix
        , stereoIn34: defDisabledStereoMix
        , stereoSpdif12: defDisabledStereoMix
        , inputMix: 1.0
        , dacMix: 1.0
        }
    , stereo2:
        { stereoDAC12: defDisabledStereoMix
        , stereoDAC34: defEnabledStereoMix
        , stereoDAC56: defDisabledStereoMix
        , stereoDAC78: defDisabledStereoMix
        , stereoIn12: defDisabledStereoMix
        , stereoIn34: defDisabledStereoMix
        , stereoSpdif12: defDisabledStereoMix
        , inputMix: 1.0
        , dacMix: 1.0
        }
    , out12ToSpdif: false
    }
