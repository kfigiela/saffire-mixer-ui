module SaffireLE.Backend where

import Logger
import SPrelude

import Control.Monad.Except (runExcept)
import Data.Bifunctor (lmap)
import Data.List (List, singleton)
import Data.List.NonEmpty (NonEmptyList(..), toList)
import Effect.Timer (setTimeout)
import Foreign (ForeignError(..), readString, renderForeignError)
import Foreign.Generic (decodeJSON, encodeJSON)
import Logger as Logger
import SaffireLE.Mixer (MixerState, defaultMixerState)
import SaffireLE.Server (SaffireStatus(..), UICmd(..))
import SaffireLE.Status (DeviceStatus, defaultDeviceStatus)
import Specular.Callback (mkCallback)
import Specular.Dom.Widget (Widget)
import Specular.FRP (newDynamic)
import Web.Event.Event (Event)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.Socket.Event.EventTypes (onClose, onError, onMessage, onOpen) as WS
import Web.Socket.Event.MessageEvent (MessageEvent, fromEvent, data_)
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.WebSocket (create, sendString, toEventTarget) as WS

type Backend =
  { meters :: Dynamic DeviceStatus
  , state :: Dynamic MixerState
  , updateState :: Callback MixerState
  , persistState :: Callback Unit
  }


connectToBackend :: String -> Callback Unit -> Widget (Dynamic (Maybe Backend))
connectToBackend url retryConnection = do
  {dynamic: backend, set: updateBackend} <- newDynamic (Nothing :: Maybe Backend)

  {dynamic: meters, set: setMeters} <- newDynamic defaultDeviceStatus
  {dynamic: state,  set: setState} <- newDynamic defaultMixerState

  let handleMessage evt = do
        let eitherMsg :: Either (NonEmptyList ForeignError) SaffireStatus
            eitherMsg = runExcept $ decodeJSON evt
        case eitherMsg of
          Right msg -> case msg of
            Meters {contents: metersData} -> setMeters metersData
            CurrentState {contents: stateData} -> setState stateData
          Left error -> Logger.error $ show error

  ws <- liftEffect $ WS.create url []

  let updateState :: Callback MixerState
      updateState = mkCallback $ \mixerCmd -> WS.sendString ws $ encodeJSON $ UpdateState { state: mixerCmd }
  let persistState :: Callback Unit
      persistState = mkCallback $ \_ -> WS.sendString ws $ encodeJSON PersistState

  liftEffect do
    onMsg ws handleMessage (Logger.error ∘ show)

    onCloseL <- eventListener \_ -> do
      Logger.error "onClose"
      updateBackend Nothing
      setTimeout 1_000 $ do
        Logger.error "retryClose"
        triggerCallback retryConnection unit
    addEventListener WS.onClose onCloseL false (WS.toEventTarget ws)

    onErrorL <- eventListener \_ -> do
      Logger.error "onError"
    addEventListener WS.onError onErrorL false (WS.toEventTarget ws)

    onOpenL <- eventListener \_ -> updateBackend $ Just {meters, state, updateState, persistState}
    addEventListener WS.onOpen onOpenL false (WS.toEventTarget ws)

  pure backend

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
