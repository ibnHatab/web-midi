module MidiConnector where
{-|

-}

import Effects exposing (Effects)
import Task exposing (Task, succeed)
import Signal exposing (Signal, Address)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Dict exposing (..)

import WebMidi exposing (..)

import Debug


-- MODEL
type Dir = In | Out

type alias Port
  = { id : ID
    , dev : MIDIPort
    , dir : Dir
    , connected : Bool
    }


type alias Model
  = { inputs  : List Port
    , outputs : List Port
    , channel : Signal (List ChannelMessage)
    , system  : Signal SystemMessage
    , error   : Maybe String
    }


init : Signal (List ChannelMessage)
     -> Signal SystemMessage
     -> (Model, Effects Action)
init chan sys =
  ( { inputs = []
    , outputs = []
    , channel = chan
    , system = sys
    , error = Nothing
    }
  , getMidiAccess
  )


-- UPDATE

type Action
  = NewMidiAccess (Maybe MIDIAccess)
  | Connect Port
  | ConnectAllInputs Bool
  | EnableInput (Maybe ID)
  | EnableOutput (Maybe ID)
  | Disconnect Port
  | DisablePort (Maybe ID)
  | OnChange ID



update : Action -> Model -> (Model, Effects Action)
update action model =
    case action |> Debug.log "act_conn" of
      NewMidiAccess Nothing ->
        ({model | error = (Just "Midi not supported") }, Effects.none)

      NewMidiAccess (Just midiAccess) ->
        ( { model |
            inputs  = Dict.foldr (\id prt prts ->
                                    (Port id prt In False) :: prts ) [] midiAccess.inputs
          , outputs = Dict.foldr (\id prt prts ->
                                    (Port id prt Out False) :: prts ) [] midiAccess.outputs
          }
        , Effects.none
        )


      Connect prt ->
        (model, case prt.dir of
                  In -> enableInput prt.id
                  Out -> enableOutput prt.id model.channel model.system
        )
      ConnectAllInputs flag ->
        ( model
        , model.inputs
          |> List.map .id
          |> List.map (if flag then enableInput else disablePort)
          |> Effects.batch)

      EnableInput (Just id) ->
        ({model |
          inputs = updatePorts id (\p -> {p | connected = True }) model.inputs
         }
        , Effects.none)
      EnableInput Nothing ->
        ( { model | error = Just "Problem accessing input device!" }, Effects.none)

      EnableOutput (Just id) ->
         let toDiconnect = List.filter .connected model.outputs
             toUpdate = updatePorts id (\p -> {p | connected = True }) model.outputs
         in ( {model | outputs = toUpdate  }
            , List.map .id toDiconnect
              |> List.map disablePort
              |> Effects.batch )

      EnableOutput Nothing ->
        ( { model | error = Just "Problem accessing output device!" }, Effects.none)


      Disconnect prt ->
        (model, disablePort prt.id)


      DisablePort (Just id) ->
        ( {model |
           inputs = updatePorts id (\p -> {p | connected = False }) model.inputs
         , outputs = updatePorts id (\p -> {p | connected = False }) model.outputs
          }
        , Effects.none )

      DisablePort Nothing ->
        (model, Effects.none)

      OnChange it ->
        (model, Effects.none)


updatePorts : ID -> (Port -> Port) ->  List Port -> List Port
updatePorts id fn prts =
  List.map (\prt -> if id == prt.id then fn prt else prt) prts

-- VIEW

(=>) = (,)

view : Signal.Address Action -> Model -> Html
view address model =
  div [ style [ "width" => "200px" ] ]
    [ h2 [headerStyle] [text "Connector"]
    , div [ class "access-error"
          , style [ ("visibility", if model.error == Nothing then "hidden" else "visible") ]]
      [ text (Maybe.withDefault "OK" model.error) ]

    , inputDeviceList address model.inputs
    , h3 [] [text "Output"]
    ]

inputDeviceList : Address Action -> List Port -> Html
inputDeviceList address ports =
  let cssVisibility = if List.isEmpty ports then "hidden" else "visible"
      allCompleted = List.all .connected ports
  in
    section
    [ id "main"
    , style [ ("visibility", cssVisibility) ]
    ]
      [ h3 [] [text "Inputs"]
      , input
          [ id "toggle-all"
          , type' "checkbox"
          , name "toggle"
          , checked allCompleted
          , onClick address (ConnectAllInputs (not allCompleted))
          ]
          []
      , label
          [ for "toggle-all" ]
          [ text "Connect all inputs" ]
      , ul
          [ id "dev-list" ]
          (List.map (deviceItem address) ports)
      ]


deviceItem : Address Action -> Port -> Html
deviceItem address prt =
  li []
       [ div
          [ class "view" ]
          [ input
            [ class "toggle"
            , type' "checkbox"
            , checked prt.connected
            , onClick address (if not prt.connected then Connect prt
                               else Disconnect prt)
            ]
            []
          , label
            [  ]
            [ text prt.dev.name ]
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

enableInput : ID -> Effects Action
enableInput id =
  WebMidi.enableInput id
         |> Task.toMaybe
         |> Task.map EnableInput
         |> Effects.task

enableOutput : ID
             -> Signal (List ChannelMessage)
             -> Signal SystemMessage
             -> Effects Action
enableOutput id chan sys  =
  WebMidi.enableOutput id chan sys
         |> Task.toMaybe
         |> Task.map EnableOutput
         |> Effects.task

disablePort : ID -> Effects Action
disablePort id =
  WebMidi.close id
         |> Task.toMaybe
         |> Task.map DisablePort
         |> Effects.task
