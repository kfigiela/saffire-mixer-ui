
module SaffireLE.Mixer.HiRes where

import SPrelude


import SaffireLE.Mixer.LowRes (ChannelMix)

type MixValue = Number

-- | 88.2 kHz and 96 kHz sample rate mixers
type StereoMixer =
    { stereo1      :: Mix
    , stereo2      :: Mix
    , recMix       :: RecMix
    , out12ToSpdif :: Boolean
    }

type Mix =
    { dac12  :: ChannelMix
    , dac34  :: ChannelMix
    , recMix :: ChannelMix
    }

type RecMixValue =
  { enabled :: Boolean
  , volume :: MixValue
  }

type RecMix =
    { in1    :: RecMixValue
    , in2    :: RecMixValue
    , in3    :: RecMixValue
    , in4    :: RecMixValue
    , spdif1 :: RecMixValue
    , spdif2 :: RecMixValue
    }

defaultMixer :: StereoMixer
defaultMixer =
    { stereo1:
        { dac12: { volume: 1.0, balance: 0.0, enabled: true}
        , dac34: { volume: 1.0, balance: 0.0, enabled: false}
        , recMix: { volume: 1.0, balance: 0.0, enabled: false}
        }
    , stereo2:
        { dac12: { volume: 1.0, balance: 0.0, enabled: false}
        , dac34: { volume: 1.0, balance: 0.0, enabled: true}
        , recMix: { volume: 1.0, balance: 0.0, enabled: false}
        }
    , recMix:
        { in1: { enabled: false, volume: 0.0 }
        , in2: { enabled: false, volume: 0.0 }
        , in3: { enabled: false, volume: 0.0 }
        , in4: { enabled: false, volume: 0.0 }
        , spdif1: { enabled: false, volume: 0.0 }
        , spdif2: { enabled: false, volume: 0.0 }
        }
    , out12ToSpdif: false
    }
