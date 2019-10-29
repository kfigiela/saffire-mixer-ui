
module SaffireLE.Mixer.HiRes where

import SPrelude

type MixValue = Number

-- | 88.2 kHz and 96 kHz sample rate mixers
newtype Mixer
    = Mixer
    { out1         :: LMix
    , out2         :: RMix
    , out3         :: LMix
    , out4         :: RMix
    , recMix       :: RecMix
    , out12ToSpdif :: Boolean
    }

derive instance newtypeMixer :: Newtype Mixer _
derive instance genericMixer :: Generic Mixer _
instance showMixer :: Show Mixer where
  show = genericShow
instance eqMixer :: Eq Mixer where
  eq = genericEq
instance decodeMixer :: Decode Mixer where
  decode = genericDecode encodingOpts
instance encodeMixer :: Encode Mixer where
  encode = genericEncode encodingOpts

newtype LMix
    = LMix
    { dac1   :: MixValue
    , dac3   :: MixValue
    , recMix :: MixValue
    }

derive instance newtypeLMix :: Newtype LMix _
derive instance genericLMix :: Generic LMix _
instance showLMix :: Show LMix where
  show = genericShow
instance eqLMix :: Eq LMix where
  eq = genericEq
instance decodeLMix :: Decode LMix where
  decode = genericDecode encodingOpts
instance encodeLMix  :: Encode LMix where
  encode = genericEncode encodingOpts

newtype RMix
    = RMix
    { dac2   :: MixValue
    , dac4   :: MixValue
    , recMix :: MixValue
    }

derive instance newtypeRMix :: Newtype RMix _
derive instance genericRMix :: Generic RMix _
instance showRMix :: Show RMix where
  show = genericShow
instance eqRMix :: Eq RMix where
  eq = genericEq
instance decodeRMix :: Decode RMix where
  decode = genericDecode encodingOpts
instance encodeRMix  :: Encode RMix where
  encode = genericEncode encodingOpts

newtype RecMix
    = RecMix
    { in1    :: MixValue
    , in2    :: MixValue
    , in3    :: MixValue
    , in4    :: MixValue
    , spdif1 :: MixValue
    , spdif2 :: MixValue
    }

derive instance newtypRecMix :: Newtype RecMix _
derive instance genericRecMix :: Generic RecMix _
instance showRecMix :: Show RecMix where
  show = genericShow
instance eqRecMix :: Eq RecMix where
  eq = genericEq
instance decodeRecMix :: Decode RecMix where
  decode = genericDecode encodingOpts
instance encodeRecMix :: Encode RecMix where
  encode = genericEncode encodingOpts

defaultMixer :: Mixer
defaultMixer =
    Mixer
    { out1:
        LMix
        { dac1: 0.0
        , dac3: 0.0
        , recMix: 0.0
        }
    , out2:
        RMix
        { dac2: 0.0
        , dac4: 0.0
        , recMix: 0.0
        }
    , out3:
        LMix
        { dac1: 0.0
        , dac3: 0.0
        , recMix: 0.0
        }
    , out4:
        RMix
        { dac2: 0.0
        , dac4: 0.0
        , recMix: 0.0
        }
    , recMix:
        RecMix
        { in1: 0.0
        , in2: 0.0
        , in3: 0.0
        , in4: 0.0
        , spdif1: 0.0
        , spdif2: 0.0
        }
    , out12ToSpdif: false
    }
