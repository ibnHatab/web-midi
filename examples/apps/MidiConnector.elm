module MidiConnector where
{-|

-}

import Effects exposing (Effects)
import Task
import Signal exposing (Signal, Address)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Set exposing (..)
import Dict exposing (..)

import WebMidi exposing (..)

import Debug

-- MODEL
type alias Model =
    { inputs  : Dict ID MIDIPort
    , outputs : Dict ID MIDIPort
    , connected : Set ID
    , error : Maybe String
    }


init : (Model, Effects Action)
init =
  ( { inputs = Dict.empty
    , outputs = Dict.empty
    , connected = Set.empty
    , error = Nothing
    }
  , getMidiAccess
  )


-- UPDATE

type Action
  = NewMidiAccess (Maybe MIDIAccess)
  | Connect ID
  | Disconnect ID
--  | OnChange

update : Action -> Model -> (Model, Effects Action)
update action model =
    case action |> Debug.log "act_conn" of
      NewMidiAccess Nothing ->
--        (model, Effects.none)
        ({model | error = (Just "Midi not supported") }, Effects.none)

      NewMidiAccess (Just midiAccess) ->
        ({model | inputs = midiAccess.inputs, outputs = midiAccess.outputs }, Effects.none)

      Connect id ->
        (model, Effects.none)
      Disconnect id ->
        (model, Effects.none)

-- VIEW

(=>) = (,)

view : Signal.Address Action -> Model -> Html
view address model =
  div [ style [ "width" => "200px" ] ]
    [ h2 [headerStyle] [text "Connector"]
    , div [ class "access-error" ] [ text "model.errors.password" ]
    , h3 [] [text "Input"]
    , ul [ id "input-dev"]
         (List.map (deviceItem address) (Dict.toList model.inputs))
    , h3 [] [text "Output"]
    ]

deviceItem : Address Action -> (ID, MIDIPort) -> Html
deviceItem address (id, p) =
  li []
       [ div
          [ class "view" ]
          [ input
            [ class "toggle"
            , type' "checkbox"
            , checked False
            , onClick address (Connect id)
            ]
            []
          , label
            [  ]
            [ text p.name ]
          ]
      ]


headerStyle : Attribute
headerStyle =
  style
    [ "width" => "200px"
    , "text-align" => "center"
    ]

-- EFFECTS

getMidiAccess : Effects Action
getMidiAccess =
  WebMidi.requestMIDIAccess defaultSettings
         |> Task.toMaybe
         |> Task.map NewMidiAccess
         |> Effects.task
