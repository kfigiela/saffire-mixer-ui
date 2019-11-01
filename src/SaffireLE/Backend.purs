module SaffireLE.Backend where

import SPrelude

import Control.Monad.Except (runExcept)
import Data.Bifunctor (lmap)
import Data.List (List, singleton)
import Data.List.NonEmpty (toList)
import Foreign (readString, renderForeignError)
import Foreign.Generic (decodeJSON, encodeJSON)
import Logger as Logger
import SaffireLE.Mixer (MixerState(..), defaultMixerState)
import SaffireLE.Server (SaffireStatus(..))
import SaffireLE.Status (DeviceStatus, defaultDeviceStatus)
import Specular.Callback (mkCallback)
import Specular.Dom.Widget (Widget)
import Specular.FRP (newDynamic)
import Web.Event.Event (Event)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.Socket.Event.EventTypes (onMessage) as WS
import Web.Socket.Event.MessageEvent (MessageEvent, fromEvent, data_)
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.WebSocket (create, sendString, toEventTarget) as WS

type Backend = {meters :: Dynamic DeviceStatus, state :: Dynamic MixerState, updateState :: Callback MixerState}

connectToBackend :: String -> Widget Backend
connectToBackend url = do
  {dynamic: meters, set: setMeters} <- newDynamic defaultDeviceStatus
  {dynamic: state,  set: setState} <- newDynamic defaultMixerState

  let handleMessage evt = do
        let msg :: Maybe SaffireStatus
            msg = hush $ runExcept $ decodeJSON evt
        whenJust msg $ case _ of
          Meters metersData -> setMeters metersData
          CurrentState stateData -> setState stateData

  ws <- liftEffect $ WS.create url []
  liftEffect $ onMsg ws handleMessage (Logger.error ∘ show)
  -- TODO: Reconnect

  let updateState :: Callback MixerState
      updateState = mkCallback $ \mixerCmd -> do
        WS.sendString ws $ encodeJSON mixerCmd
  pure {meters, state, updateState}

onMsg :: WebSocket -> (String -> Effect Unit) -> (List String -> Effect Unit) -> Effect Unit
onMsg ws success failure = do
    l <- eventListener \x -> either failure success $ readEvent x
    addEventListener WS.onMessage l useCapture target
  where
  readEvent :: Event -> Either (List String) String
  readEvent e = (messageEvent e) >>= string
  string :: MessageEvent -> Either (List String) String
  string = lmap (map renderForeignError <<< toList) <<< runExcept <<< readString <<< data_
  messageEvent :: Event -> Either (List String) MessageEvent
  messageEvent = maybe (Left $ singleton "Can't get event") Right <<< fromEvent
  useCapture = false
  target = WS.toEventTarget ws
