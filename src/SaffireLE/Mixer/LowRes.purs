module SaffireLE.Mixer.LowRes where

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

---

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
toggleChannelMix (MonoPair { ch1Volume:  { volume: v1, enabled: e1 }, ch2Volume: { volume: v2, enabled: e2 } }) =
  StereoPair
  { ch12Volume:
    { volume: (v1 + v2)/2.0
    , balance: 0.0
    , enabled: e1 || e2
    }
  }
toggleChannelMix (StereoPair { ch12Volume: { volume, enabled } }) =
  MonoPair
  { ch1Volume: { volume, enabled, balance: 0.0 }
  , ch2Volume: { volume, enabled, balance: 0.0 }
  }
