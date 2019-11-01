module SaffireLE.Server where

import SPrelude

import SaffireLE.Mixer as M
import SaffireLE.Status (DeviceStatus)

data SaffireStatus =
    Meters DeviceStatus
    | CurrentState M.MixerState

derive instance genericSaffireStatus :: Generic SaffireStatus _
instance showSaffireStatus :: Show SaffireStatus where
  show = genericShow
instance eqSaffireStatus :: Eq SaffireStatus where
  eq = genericEq
instance decodeSaffireStatus :: Decode SaffireStatus where
  decode = genericDecode encodingOpts
