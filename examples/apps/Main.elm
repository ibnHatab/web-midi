
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
    (connector, connectorFx) = MidiConnector.init midiOut.signal sysOut.signal loopback.address
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
  | Loopback String
  | None

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

    Loopback it ->
      ({ model | midiConnector = fst (MidiConnector.update (MidiConnector.OnChange it) model.midiConnector)},
      Effects.none)

    None ->
      (model, Effects.none)

inputs : List (Signal Action)
inputs = []

-- VIEW

(=>) = (,)


view : Signal.Address Action -> Model -> Html.Html
view address model =
  div [ style [ "display" => "flex", "flex-wrap" => "wrap" ] ]
      [ MidiConnector.view (Signal.forwardTo address Connector) model.midiConnector
      , Piano.view (Signal.forwardTo address Piano) model.piano
      ]

-- APP

app =
  StartApp.start
             { init = init
             , update = update
             , view = view
             , inputs = [events]
             }

main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks

-- SIGNALS

midiOut : Signal.Mailbox (List ChannelMessage)
midiOut =
  Signal.mailbox [initChannelMsg]

sysOut : Signal.Mailbox SystemMessage
sysOut =
  Signal.mailbox initSystemMsg

port midiOutPort : Signal (List ChannelMessage)
port midiOutPort = midiOut.signal

port sysOutPort : Signal SystemMessage
port sysOutPort = sysOut.signal

loopback : Signal.Mailbox String
loopback =
  Signal.mailbox ""

events = Signal.map (\it -> Loopback (it |> Debug.log "it")) loopback.signal
