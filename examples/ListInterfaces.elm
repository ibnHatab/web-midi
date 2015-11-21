

import WebMidi exposing (..)
import MidiEvent exposing (..)

import Html exposing (..)
import Graphics.Element exposing (show)
import Dict exposing (Dict, empty, get)
import Task exposing (Task, andThen, succeed)
import Maybe exposing (withDefault)
import Debug

synch = "Synth input port (16600:0)"
keyboard = "Virtual Keyboard"

selectInstrument : String -> Dict ID MIDIPort -> Maybe ID
selectInstrument name instruments =
  let ids = Dict.foldr (\key val keyList ->
                            if val.name == name then key :: keyList
                            else keyList) [] instruments
  in List.head ids

midiOut : Signal.Mailbox MidiEvent
midiOut =
  Signal.mailbox none

port midiOutPort : Signal MidiEvent
port midiOutPort = midiOut.signal

port midiInPort : Signal MidiEvent
port midiInPort = Signal.constant none

port midiAccess : Task x ()
port midiAccess =
  WebMidi.requestMIDIAccess defaultSettings
           `andThen` \midi -> let id = withDefault "unknown" (selectInstrument synch midi.outputs)
                              in WebMidi.open id midiOut.signal
           `andThen` \outPort -> let id = withDefault "unknown" (selectInstrument keyboard midi.inputs)
                                 in WebMidi.open id midiInPort
           `andThen` \p -> Signal.send midiOut.address (MidiNote True (1,4) 0 0 0)
           `andThen` \out -> report midi


readme : Signal.Mailbox MIDIAccess
readme =
  Signal.mailbox (MIDIAccess Dict.empty Dict.empty True)

report : MIDIAccess -> Task x ()
report markdown =
  Signal.send readme.address markdown


--main : Html
-- main =
--   Signal.map show readme.signal

main =
  Signal.map show midiInPort
