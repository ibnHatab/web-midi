
import Effects exposing (Effects, map, batch, Never)
import Html exposing (..)
import Html.Attributes exposing (style)
import StartApp
import Task

import WebMidi exposing (..)

import Piano exposing (Action, view, init)
import MidiConnector exposing (Action, view, init)

import Char exposing (KeyCode, toUpper)
import Set exposing (Set, map)
import Keyboard exposing (..)
import String exposing (..)
import Dict exposing (..)

import Debug

-- MODEL
type alias Model =
  {
    midiConnector: MidiConnector.Model
  , piano : Piano.Model
  }

pianoKeyMap = Piano.defaultMap0

init : (Model, Effects Action)
init =
  let
    (connector, connectorFx) = MidiConnector.init midiOut.signal sysOut.signal
    (piano, pianoFx) = Piano.init 2 4 (Just pianoKeyMap) midiOut.address
  in
    ( Model connector piano
    , Effects.batch
               [ Effects.map Connector connectorFx
               , Effects.map Piano pianoFx
               ]
    )

-- UPDATE
type Action
  = Connector MidiConnector.Action
  | Piano Piano.Action

update : Action -> Model -> (Model, Effects Action)
update message model =
  case message -- |> Debug.log "main_act"
  of
    Connector act ->
      let
        (conn, fx) = MidiConnector.update act model.midiConnector
      in
        ( {model | midiConnector = conn}
        , Effects.map Connector fx
        )
    Piano act ->
      let
        (piano, fx) = Piano.update act model.piano
      in
        ( {model | piano = piano}
        , Effects.map Piano fx
        )


-- SIGNALS
midiOut : Signal.Mailbox (List ChannelMessage)
midiOut =
  Signal.mailbox [initChannelMsg]

port midiOutPort : Signal (List ChannelMessage)
port midiOutPort = midiOut.signal

sysOut : Signal.Mailbox SystemMessage
sysOut =
  Signal.mailbox initSystemMsg

port sysOutPort : Signal SystemMessage
port sysOutPort = sysOut.signal

inputs : List (Signal Action)
inputs = [events, keyPressed]

events : Signal Action
events = Signal.map (Connector << MidiConnector.OnChange) WebMidi.onChange


dropMap : (a -> b) -> Signal a -> Signal b
dropMap f signal =
  Signal.dropRepeats (Signal.map f signal)


pianoKeyToPitch = Piano.expandKeyMap pianoKeyMap

keyPressed : Signal Action
keyPressed = dropMap (\chars -> if not (Set.member 16 chars) -- if Shift not pressed
                                then Set.map (Char.toLower << Char.fromCode) chars
                                else Set.map Piano.fromCodeSpetial chars
                     ) Keyboard.keysDown

             |> Signal.map (Set.foldr (\c acc -> case Dict.get c pianoKeyToPitch of
                                                   Nothing ->
                                                     acc
                                                   Just p ->
                                                     p :: acc
                                      ) [] )
             |> Signal.map (Piano << Piano.KeysDown)

-- VIEW
(=>) : a -> b -> ( a, b )
(=>) = (,)

view : Signal.Address Action -> Model -> Html.Html
view address model =
  div [ style [ "display" => "flex", "flex-wrap" => "wrap" ] ]
      [ MidiConnector.view (Signal.forwardTo address Connector) model.midiConnector
      , Piano.view (Signal.forwardTo address Piano) model.piano
      ]

-- APP
app : StartApp.App Model
app =
  StartApp.start
             { init = init
             , update = update
             , view = view
             , inputs = inputs
             }

main : Signal Html
main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
