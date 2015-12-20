
import Char exposing (KeyCode, toUpper)
import Set exposing (Set, map)
import Keyboard exposing (..)
import Dict exposing (..)

import Effects exposing (Effects, map, batch, Never)
import Html exposing (..)
import Html.Attributes exposing (style)
import StartApp
import Task

import WebMidi exposing (..)
import MidiEvent exposing (..)

import Music exposing (..)

import Piano exposing (Action, view, init)
import MidiConnector exposing (Action, view, init)
import IntervalAnimation exposing (Action, view, init)


-- import Debug

-- MODEL
type alias Model =
  {
    midiConnector : MidiConnector.Model
  , piano         : Piano.Model
  , intervals     : IntervalAnimation.Model
  , keyCodes      : Set KeyCode
  }

pianoKeyMap : Piano.PianoKeyMap
pianoKeyMap = Piano.defaultMap0

init : (Model, Effects Action)
init =
  let
    (connector, connectorFx) = MidiConnector.init midiOut.signal sysOut.signal
    (piano, pianoFx) = Piano.init 2 5 (Just pianoKeyMap) midiOut.address
    (intervals, intervalsFx) = IntervalAnimation.init 2 5
  in
    ( Model connector piano intervals Set.empty
    , Effects.batch
               [ Effects.map Connector connectorFx
               , Effects.map Piano pianoFx
               , Effects.map IntervalAnimation intervalsFx
               ]
    )

-- UPDATE
type Action
  = Connector MidiConnector.Action
  | Piano Piano.Action
  | Keyboard (Set KeyCode)
  | IntervalAnimation IntervalAnimation.Action
  | NoOp

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
    IntervalAnimation act ->
      let
        (intervals, fx) = IntervalAnimation.update act model.intervals
      in
        ( {model | intervals = intervals}
        , Effects.map IntervalAnimation fx
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
                        else (piano, Effects.none)
      in
      ( { model | keyCodes = keyCodes, piano = piano' }
      , Effects.batch
                 [ Effects.map Piano fx
                 , Effects.map Piano fx'
                 ]
      )
    NoOp ->
      (model, Effects.none)

pianoKeyToPitch : Dict Char Music.Pitch
pianoKeyToPitch = Piano.expandKeyMap pianoKeyMap

keyEventToChar : Bool -> Set Int -> Set Char
keyEventToChar shift ev =
  Set.filter (\c -> c > 46 && c <= 90) ev -- keep ASCII only
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
inputs : List (Signal Action)
inputs = [midiOnChange, keyPressed, midiEvents]

midiOnChange : Signal Action
midiOnChange = Signal.map (Connector << MidiConnector.OnChange) WebMidi.onChange

keyPressed : Signal Action
keyPressed =
  Signal.dropRepeats Keyboard.keysDown
    |> Signal.map Keyboard


midiEvents : Signal Action
midiEvents =
  Signal.map (\e -> case decodeChannelEvent e of
                      (t, NoteOn ch p v) ->
                        Piano (Piano.PithOn [pitch p])
                      (t, NoteOff  ch p v) ->
                        Piano (Piano.PithOff [pitch p])
                      otherwise ->
                          NoOp
             ) WebMidi.channel

-- Multiplexed output
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



-- VIEW
(=>) : a -> b -> ( a, b )
(=>) = (,)

view : Signal.Address Action -> Model -> Html.Html
view address model =
  div [ style [ "display" => "flex", "flex-wrap" => "wrap" ] ]
      [ MidiConnector.view (Signal.forwardTo address Connector) model.midiConnector
      , div [ style [ "flex-wrap" => "wrap" ] ]
            [
             IntervalAnimation.view (Signal.forwardTo address IntervalAnimation)
                               model.intervals
            , Piano.view (Signal.forwardTo address Piano) model.piano
            ]

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
