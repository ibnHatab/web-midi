
import Effects exposing (Effects, map, batch, Never)
import Html exposing (..)
import Html.Attributes exposing (style)
import StartApp
import Task

import WebMidi exposing (..)

import Piano exposing (Action, view, init)
import Music exposing (Pitch)
import MidiConnector exposing (Action, view, init)

import Char exposing (KeyCode, toUpper)
import Set exposing (Set, map)
import Keyboard exposing (..)
import Dict exposing (..)

-- import Debug

-- MODEL
type alias Model =
  {
    midiConnector: MidiConnector.Model
  , piano : Piano.Model
  , keyCodes : Set KeyCode
  }

pianoKeyMap : Piano.PianoKeyMap
pianoKeyMap = Piano.defaultMap0

init : (Model, Effects Action)
init =
  let
    (connector, connectorFx) = MidiConnector.init midiOut.signal sysOut.signal
    (piano, pianoFx) = Piano.init 2 4 (Just pianoKeyMap) midiOut.address
  in
    ( Model connector piano Set.empty
    , Effects.batch
               [ Effects.map Connector connectorFx
               , Effects.map Piano pianoFx
               ]
    )

-- UPDATE
type Action
  = Connector MidiConnector.Action
  | Piano Piano.Action
  | Keyboard (Set KeyCode)

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
    Keyboard keyCodes ->
      let
        shift = Set.member 16 keyCodes
        pressed = Set.diff keyCodes model.keyCodes
                     |> keyEventToChar shift
                     |> charToPitch
        lifted = Set.diff model.keyCodes keyCodes
                     |> keyEventToChar shift
                     |> charToPitch

        (piano, fx) = if not (List.isEmpty pressed)
                      then Piano.update (Piano.PithOn pressed) model.piano
                      else (model.piano, Effects.none)

        (piano', fx') = if not (List.isEmpty lifted)
                        then Piano.update (Piano.PithOff lifted) piano
                        else (model.piano, Effects.none)
      in
      ( { model | keyCodes = keyCodes, piano = piano' }
      , Effects.batch
                 [ Effects.map Piano fx
                 , Effects.map Piano fx'
                 ]
      )


pianoKeyToPitch : Dict Char Music.Pitch
pianoKeyToPitch = Piano.expandKeyMap pianoKeyMap

keyEventToChar : Bool -> Set Int -> Set Char
keyEventToChar shift ev =
  Set.filter (\c -> c > 46 && c <= 90) ev -- keep ASCII only
    |> Debug.log "ev1"
    |> if not shift
       then Set.map (Char.toLower << Char.fromCode)
       else Set.map Piano.fromCodeSpetial

charToPitch : Set Char -> List Pitch
charToPitch = Set.foldr (\c acc -> case Dict.get c pianoKeyToPitch of
                                     Nothing ->
                                       acc
                                     Just p ->
                                       p :: acc
                        ) []

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

keyPressed : Signal Action
keyPressed =
  dropMap (Keyboard) Keyboard.keysDown



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
