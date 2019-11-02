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


data UICmd  =
    UpdateState M.MixerState
    | PersistState

derive instance genericUICmd  :: Generic UICmd  _
instance showUICmd  :: Show UICmd  where
  show = genericShow
instance eqUICmd :: Eq UICmd  where
  eq = genericEq
instance encodeUICmd :: Encode UICmd  where
  encode = genericEncode encodingOpts
