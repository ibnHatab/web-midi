
import Effects exposing (Effects, map, batch, Never)
import Html exposing (..)
import Html.Attributes exposing (style)
import StartApp
import Task

import WebMidi exposing (..)

import Piano exposing (Action, view, init)
import MidiConnector exposing (Action, view, init)

import Debug


-- MODEL
type alias Model =
  {
    midiConnector: MidiConnector.Model
  , piano : Piano.Model
  }

init : (Model, Effects Action)
init =
  let
    (connector, connectorFx) = MidiConnector.init midiOut.signal sysOut.signal
    (piano, pianoFx) = Piano.init
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
  case message |> Debug.log "main_act"
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
inputs = [events]

events : Signal Action
events = Signal.map (Connector << MidiConnector.OnChange) WebMidi.onChange

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
