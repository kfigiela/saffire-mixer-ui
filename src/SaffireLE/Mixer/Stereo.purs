module SaffireLE.Mixer.Stereo where

import SPrelude

type MixValue = Number

newtype StereoMixer
    = StereoMixer
    { stereo1      :: StereoMix
    , stereo2      :: StereoMix
    , out12ToSpdif :: Boolean
    }

derive instance newtypeStereoMixer :: Newtype StereoMixer _
derive instance genericStereoMixer :: Generic StereoMixer _
instance showStereoMixer :: Show StereoMixer where
  show = genericShow
instance eqStereoMixer :: Eq StereoMixer where
  eq = genericEq
instance decodeStereoMixer :: Decode StereoMixer where
  decode = genericDecode encodingOpts
instance encodeStereoMixer :: Encode StereoMixer where
  encode = genericEncode encodingOpts

newtype StereoMix
    = StereoMix
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

derive instance newtypeStereoMix :: Newtype StereoMix _
derive instance genericStereoMix :: Generic StereoMix _
instance showStereoMix :: Show StereoMix where
  show = genericShow
instance eqStereoMix :: Eq StereoMix where
  eq = genericEq
instance decodeStereoMix :: Decode StereoMix where
  decode = genericDecode encodingOpts
instance encodeStereoMix :: Encode StereoMix where
  encode = genericEncode encodingOpts

-- | Represents mix of stereo channel
newtype ChannelMix
    = ChannelMix
    { volume  :: MixValue
    , balance :: MixValue
    , enabled :: Boolean
    }

derive instance newtypeChannelMix :: Newtype ChannelMix _
derive instance genericChannelMix :: Generic ChannelMix _
instance showChannelMix :: Show ChannelMix where
  show = genericShow
instance eqChannelMix :: Eq ChannelMix where
  eq = genericEq
instance decodeChannelMix :: Decode ChannelMix where
  decode = genericDecode encodingOpts
instance encodeChannelMix :: Encode ChannelMix where
  encode = genericEncode encodingOpts

data StereoMixValue
    = StereoPair
    { ch12Volume :: ChannelMix
    }
    | MonoPair
    { ch1Volume :: ChannelMix
    , ch2Volume :: ChannelMix
    }

defEnabledStereoMix = StereoPair { ch12Volume: ChannelMix { volume: 1.0, balance: 0.0, enabled: true}} :: StereoMixValue
defDisabledStereoMix = StereoPair { ch12Volume: ChannelMix { volume: 0.0, balance: 0.0, enabled: false}} :: StereoMixValue
defDisabledMonoMix = MonoPair { ch1Volume: ChannelMix { volume: 0.0, balance: 0.0, enabled: false}, ch2Volume: ChannelMix { volume: 0.0, balance: 0.0, enabled: false}} :: StereoMixValue

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
    StereoMixer
    { stereo1:
        StereoMix
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
        StereoMix
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
