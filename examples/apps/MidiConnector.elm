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
    , stale : Bool
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
  | ConnectInput Port
  | ConnectOutput Port
  | ConnectAllInputs Bool
  | EnableInput (Maybe ID)
  | EnableOutput (Maybe ID)
  | Disconnect Port
  | DisablePort (Maybe ID)
  | OnChange (ID, String)


{-|
- add `new` ports after `old`
- mark some `old` ports as `stale` if not in `new`
-}
mergePorts : Dir -> List Port -> Dict ID MIDIPort -> List Port
mergePorts dir old added =
  let stale = List.map (\prt -> if Dict.member prt.id added then { prt | stale = False }
                                else { prt | stale = True })
              old
      new = Dict.foldr (\id prt prts ->
                          if List.any (((==) id) << .id) old then prts
                          else (Port id prt dir False False) :: prts)
            [] added
  in stale ++ new

updatePorts : ID -> (Port -> Port) ->  List Port -> List Port
updatePorts id fn prts =
  List.map (\prt -> if id == prt.id then fn prt else prt) prts


update : Action -> Model -> (Model, Effects Action)
update action model =
    case action |> Debug.log "act_conn"
    of
      NewMidiAccess Nothing ->
        ({model | error = (Just "Midi not supported") }, Effects.none)

      NewMidiAccess (Just midiAccess) ->
        let (lastIn, lastOut) = lastMidiUsed 42
            markConnected name ps =
              List.map (\p -> if p.dev.name == name
                              then {p | connected = True}  else p) ps

            inputs = mergePorts In model.inputs midiAccess.inputs
                    |> markConnected lastIn

            outputs = mergePorts In model.outputs midiAccess.outputs
                    |> markConnected lastOut

            fx = ( List.filter .connected inputs |> List.map (enableInput << .id))
                 ++
                 (List.filter .connected outputs
                    |> List.map (\p -> enableOutput p.id model.channel model.system))
        in
        ( { model | inputs = inputs, outputs = outputs
          }
        , Effects.batch fx
        )

      -- Handle INPUT devices; connect any or all
      ConnectInput prt ->
        ( { model | inputs = updatePorts prt.id (\p -> {p | connected = True}) model.inputs }
          , if not prt.stale
            then enableInput prt.id
            else Effects.none)

      ConnectAllInputs flag ->
        ( model
        , model.inputs
          |> List.filter (((/=) flag) << .connected)
          |> List.map (\p -> {p | connected = flag})
          |> List.map .id
          |> List.map (if flag then enableInput else disablePort)
          |> Effects.batch)

      EnableInput (Just id) ->
          ({model | error = Nothing }, Effects.none)

      EnableInput Nothing ->
        ( { model | error = Just "Problem accessing input device!" }, Effects.none)

      -- Handle OUTPUT devices; connect one
      ConnectOutput prt ->
        ( { model | outputs = List.map (\p -> { p | connected = (p == prt) }) model.outputs
          }
        , ( if not prt.stale
            then enableOutput prt.id model.channel model.system
            else Effects.none
          ) :: ( model.outputs |> List.filter .connected
                               |> List.map .id
                               |> List.map disablePort )
          |> Effects.batch
        )

      EnableOutput (Just id) ->
        ( { model | error = Nothing }, Effects.none )

      EnableOutput Nothing ->
        ( { model | error = Just "Problem accessing output device!" }, getMidiAccess)

      Disconnect prt ->
        ( { model |
            inputs = updatePorts prt.id (\p -> {p | connected = False }) model.inputs
          , outputs = updatePorts prt.id (\p -> {p | connected = False }) model.outputs
          }
        , disablePort prt.id
        )

      DisablePort (Just id) ->
        ( { model | error = Nothing}, Effects.none )

      DisablePort Nothing ->
        ( model, getMidiAccess )  -- rescan ports in case of raise

      OnChange (id, action) ->
        case action of
          "connected" ->
            if List.any (\p -> p.id == id) (model.inputs ++ model.outputs)
            then
            ( { model |
                inputs = updatePorts id (\p -> {p | stale = False }) model.inputs
              , outputs = updatePorts id (\p -> {p | stale = False }) model.outputs
              }
            , ( model.inputs
                |> List.filter (\p -> p.id == id)
                |> List.map (\prt -> if prt.stale
                                     then enableInput prt.id
                                     else Effects.none) )
              ++ ( model.outputs
                   |> List.filter (\p -> p.id == id)
                   |> List.map (\prt -> if prt.stale && prt.connected
                                        then enableOutput prt.id model.channel model.system
                                        else Effects.none) )
              |> Effects.batch
            )
          else
            (model, getMidiAccess )
          "disconnected" ->
            ( { model |
                inputs = updatePorts id (\p -> {p | stale = True }) model.inputs
              , outputs = updatePorts id (\p -> {p | stale = True }) model.outputs
              }
            , Effects.none
            )
          otherwise ->
            (model, Effects.none)

-- VIEW
(=>) : a -> b -> ( a, b )
(=>) = (,)

view : Signal.Address Action -> Model -> Html
view address model =
  div [ style [ "width" => "200px" ] ]
    [ h2 [] [text "Connector"]
    , div [ class "access-error"
          , style [ ("visibility", if model.error == Nothing then "hidden" else "visible") ]
          ]
          [ text (Maybe.withDefault "OK" model.error) ]

    , inputDeviceList address model.inputs
    , outputDeviceList address model.outputs
    ]

inputDeviceList : Address Action -> List Port -> Html
inputDeviceList address ports =
  let
      item prt =
        li []
             [ div
               [ class "view" ]
               [ input
                 [ class "toggle"
                 , type' "checkbox"
                 , checked prt.connected
                 , onClick address (if not prt.connected then ConnectInput prt
                                    else Disconnect prt)
                 ]
                 []
               , label
                 (if prt.stale then [ style ["text-decoration" => "line-through"] ] else [])
                 [ text prt.dev.name ]
               ]
             ]

      cssVisibility = if List.isEmpty ports then "hidden" else "visible"
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
          (List.map (item) ports)
      ]

outputDeviceList : Address Action -> List Port -> Html
outputDeviceList address ports =
  let
      item prt =
        div
          [ class "view" ]
          [ input
            [ class "select"
            , type' "radio"
            , checked prt.connected
            , readonly (not prt.stale)
            , on "change" targetChecked
                   (\_ -> Signal.message address (ConnectOutput prt))
            ]
            []
          , label
            (if prt.stale then [ style ["text-decoration" => "line-through"] ] else [])
            [ text prt.dev.name ]
          ]
      cssVisibility = if List.isEmpty ports then "hidden" else "visible"
  in
    section
    [ id "main"
    , style [ ("visibility", cssVisibility) ]
    ]
      [ h3 [] [text "Outputs"]
      , div [ class "view" ]
        (List.map (item) ports)
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
