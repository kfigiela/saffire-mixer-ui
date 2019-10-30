
module SaffireLE.Mixer.Matrix where

import SPrelude

type MixValue = Number

-- | 44.1 kHz and 48 kHz sample rate mixers
type MatrixMixer =
    { out1         :: Mix
    , out2         :: Mix
    , out3         :: Mix
    , out4         :: Mix
    , out12ToSpdif :: Boolean
    }

type Mix =
    { dac1   :: MixValue
    , dac2   :: MixValue
    , dac3   :: MixValue
    , dac4   :: MixValue
    , dac5   :: MixValue
    , dac6   :: MixValue
    , dac7   :: MixValue
    , dac8   :: MixValue
    , in1    :: MixValue
    , in2    :: MixValue
    , in3    :: MixValue
    , in4    :: MixValue
    , spdif1 :: MixValue
    , spdif2 :: MixValue
    }

--

defaultMix :: Mix
defaultMix =
    { dac1: 0.0
    , dac2: 0.0
    , dac3: 0.0
    , dac4: 0.0
    , dac5: 0.0
    , dac6: 0.0
    , dac7: 0.0
    , dac8: 0.0
    , in1: 0.0
    , in2: 0.0
    , in3: 0.0
    , in4: 0.0
    , spdif1: 0.0
    , spdif2: 0.0
    }

defaultMixer :: MatrixMixer
defaultMixer =
    { out1: defaultMix
    , out2: defaultMix
    , out3: defaultMix
    , out4:  defaultMix
    , out12ToSpdif: false
    }
