
import Html exposing (..)
import Graphics.Element exposing (show)
import Dict exposing (Dict, empty, get)
import Task exposing (Task, andThen, succeed)
import Maybe exposing (withDefault)
import Debug

import WebMidi exposing (..)
import MidiEvent exposing (..)
import Music exposing (..)

keyboard = "Virtual Keyboard"

port midiAccess : Task String ID
port midiAccess =
  WebMidi.requestMIDIAccess defaultSettings
           `andThen` \midi -> Task.fromMaybe "No device found" (selectInstrument keyboard midi.inputs)
           `andThen` \id   -> WebMidi.enableInput (id |> Debug.log "dev id" )

events = Signal.foldp (\e ex -> (decodeChannelEvent e) :: ex) [] WebMidi.channel

main = Signal.map (show << List.reverse) events
