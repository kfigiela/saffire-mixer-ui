module SaffireLE.Status where

import SPrelude

data VUMeter
    = In1
    | In3
    | SpdifIn1
    | In2
    | In4
    | SpdifIn2
    | Out1
    | Out3
    | Out5
    | SpdifOut7
    | Out2
    | Out4
    | Out6
    | SpdifOut8
    | DAC1
    | DAC3
    | DAC2
    | DAC4

derive instance genericVUMeter :: Generic VUMeter _
instance showVUMeter :: Show VUMeter where
  show = genericShow
instance eqVUMeter :: Eq VUMeter where
  eq = genericEq
instance decodeVUMeter :: Decode VUMeter where
  decode = genericDecodeEnum defaultGenericEnumOptions

type MeterValue = Maybe Number

newtype VUMeters
    = VUMeters
    { in1       :: MeterValue
    , in3       :: MeterValue
    , spdifIn1  :: MeterValue
    , in2       :: MeterValue
    , in4       :: MeterValue
    , spdifIn2  :: MeterValue
    , out1      :: MeterValue
    , out3      :: MeterValue
    , out5      :: MeterValue
    , spdifOut7 :: MeterValue
    , out2      :: MeterValue
    , out4      :: MeterValue
    , out6      :: MeterValue
    , spdifOut8 :: MeterValue
    , dac1      :: MeterValue
    , dac3      :: MeterValue
    , dac2      :: MeterValue
    , dac4      :: MeterValue
    }

derive instance newtypeVUMeters :: Newtype VUMeters _
derive instance genericVUMeters :: Generic VUMeters _
instance showVUMeters :: Show VUMeters where
  show = genericShow
instance eqVUMeters :: Eq VUMeters where
  eq = genericEq
instance decodeVUMeters :: Decode VUMeters where
  decode = genericDecode encodingOpts

newtype DeviceStatus
    = DeviceStatus
    { meters       :: VUMeters
    , extClockLock :: ExternalClockStatus
    , audioOn      :: AudioStatus
    }

derive instance newtypeDeviceStatus :: Newtype DeviceStatus _
derive instance genericDeviceStatus :: Generic DeviceStatus _
instance showDeviceStatus :: Show DeviceStatus where
  show = genericShow
instance eqDeviceStatus :: Eq DeviceStatus where
  eq = genericEq
instance decodeDeviceStatus :: Decode DeviceStatus where
  decode = genericDecode encodingOpts

defaultDeviceStatus :: DeviceStatus
defaultDeviceStatus =
    DeviceStatus
    { meters:
        VUMeters
        { in1:       Nothing
        , in3:       Nothing
        , spdifIn1:  Nothing
        , in2:       Nothing
        , in4:       Nothing
        , spdifIn2:  Nothing
        , out1:      Nothing
        , out3:      Nothing
        , out5:      Nothing
        , spdifOut7: Nothing
        , out2:      Nothing
        , out4:      Nothing
        , out6:      Nothing
        , spdifOut8: Nothing
        , dac1:      Nothing
        , dac3:      Nothing
        , dac2:      Nothing
        , dac4:      Nothing
        }
    , extClockLock: NoSignal
    , audioOn: NotConnected
    }

data ExternalClockStatus
    = NoSignal
    | Locked -- ^ not sure
    | Signal

derive instance genericExternalClockStatus :: Generic ExternalClockStatus _
instance showExternalClockStatus :: Show ExternalClockStatus where
  show = genericShow
instance eqExternalClockStatus :: Eq ExternalClockStatus where
  eq = genericEq
instance decodeExternalClockStatus :: Decode ExternalClockStatus where
  decode = genericDecodeEnum defaultGenericEnumOptions


data AudioStatus = Idle | Running | NotConnected

derive instance genericAudioStatus :: Generic AudioStatus _
instance showAudioStatus :: Show AudioStatus where
  show = genericShow
instance eqAudioStatus :: Eq AudioStatus where
  eq = genericEq
instance decodeAudioStatus :: Decode AudioStatus where
  decode = genericDecodeEnum defaultGenericEnumOptions
