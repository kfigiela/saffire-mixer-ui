
module SaffireLE.Mixer.Matrix where

import SPrelude

type MixValue = Number

-- | 44.1 kHz and 48 kHz sample rate mixers
newtype MatrixMixer
    = MatrixMixer
    { out1         :: Mix
    , out2         :: Mix
    , out3         :: Mix
    , out4         :: Mix
    , out12ToSpdif :: Boolean
    }

derive instance newtypeMatrixMixer :: Newtype MatrixMixer _
derive instance genericMatrixMixer :: Generic MatrixMixer _
instance showMatrixMixer :: Show MatrixMixer where
  show = genericShow
instance eqMatrixMixer :: Eq MatrixMixer where
  eq = genericEq
instance decodeMatrixMixer :: Decode MatrixMixer where
  decode = genericDecode encodingOpts
instance encodeMatrixMixer :: Encode MatrixMixer where
  encode = genericEncode encodingOpts

newtype Mix
    = Mix
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

derive instance newtypeMix :: Newtype Mix _
derive instance genericMix :: Generic Mix _
instance showMix :: Show Mix where
  show = genericShow
instance eqMix :: Eq Mix where
  eq = genericEq
instance decodeMix :: Decode Mix where
  decode = genericDecode encodingOpts
instance encodeMix :: Encode Mix where
  encode = genericEncode encodingOpts

--

defaultMix :: Mix
defaultMix =
    Mix
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
    MatrixMixer
    { out1: defaultMix
    , out2: defaultMix
    , out3: defaultMix
    , out4:  defaultMix
    , out12ToSpdif: false
    }
