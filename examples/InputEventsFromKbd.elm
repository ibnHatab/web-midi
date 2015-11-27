
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

port midiAccess : Task x MIDIPort
port midiAccess =
  WebMidi.requestMIDIAccess defaultSettings
           `andThen` \midi ->
             WebMidi.open (withDefault "none" (selectInstrument keyboard midi.inputs)) WebMidi.channel

events = Signal.foldp (\e ex -> (decodeChannelEvent e) :: ex) [] WebMidi.channel

main = Signal.map (show << List.reverse) events
