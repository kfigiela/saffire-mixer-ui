
module SaffireLE.Mixer.HiRes where

import SPrelude

type MixValue = Number

-- | 88.2 kHz and 96 kHz sample rate mixers
type Mixer =
    { out1         :: LMix
    , out2         :: RMix
    , out3         :: LMix
    , out4         :: RMix
    , recMix       :: RecMix
    , out12ToSpdif :: Boolean
    }

type LMix =
    { dac1   :: MixValue
    , dac3   :: MixValue
    , recMix :: MixValue
    }

type RMix =
    { dac2   :: MixValue
    , dac4   :: MixValue
    , recMix :: MixValue
    }

type RecMix =
    { in1    :: MixValue
    , in2    :: MixValue
    , in3    :: MixValue
    , in4    :: MixValue
    , spdif1 :: MixValue
    , spdif2 :: MixValue
    }

defaultMixer :: Mixer
defaultMixer =
    { out1:
        { dac1: 0.0
        , dac3: 0.0
        , recMix: 0.0
        }
    , out2:
        { dac2: 0.0
        , dac4: 0.0
        , recMix: 0.0
        }
    , out3:
        { dac1: 0.0
        , dac3: 0.0
        , recMix: 0.0
        }
    , out4:
        { dac2: 0.0
        , dac4: 0.0
        , recMix: 0.0
        }
    , recMix:
        { in1: 0.0
        , in2: 0.0
        , in3: 0.0
        , in4: 0.0
        , spdif1: 0.0
        , spdif2: 0.0
        }
    , out12ToSpdif: false
    }
