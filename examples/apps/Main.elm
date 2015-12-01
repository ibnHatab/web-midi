
import Effects exposing (Effects, map, batch, Never)
import Html exposing (..)
import Html.Attributes exposing (style)
import StartApp

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
    (connector, connectorFx) = MidiConnector.init
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
  = Create
  | Connector MidiConnector.Action
  | Piano Piano.Action

update : Action -> Model -> (Model, Effects Action)
update message model =
  case message |> Debug.log "m_act" of
    Create ->
      (model, Effects.none)
    otherwise ->
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
             , inputs = []
             }

main =
  app.html
