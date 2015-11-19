

import WebMidi exposing (..)
import Html exposing (..)
import Graphics.Element exposing (show)
import Dict exposing (Dict, empty, get)
import Task exposing (Task, andThen, succeed)

import Debug

synch = "1F01E2F5CAA14D73601F9D2DCD2521E51B4480AE41379B0116CE6970B0BC62A7"

midiOut : Signal.Mailbox MidiNote
midiOut =
  Signal.mailbox none

port midiOutPort : Signal MidiNote
port midiOutPort = midiOut.signal

keyboard = "025EB0F17430BBAF69BB5190FE0E46602E9C64C00FF8431A47170C1B7D175A95"
port midiInPort : Signal MidiNote
port midiInPort = Signal.constant none

port midiAccess : Task x ()
port midiAccess =
  WebMidi.requestMIDIAccess defaultSettings
           `andThen` \midi -> WebMidi.open synch midiOut.signal
           `andThen` \outPort -> WebMidi.open keyboard midiInPort
           `andThen` \p -> Signal.send midiOut.address (MidiNote True (1,4) 0 0 0)
           `andThen` \out -> report midi


readme : Signal.Mailbox MIDIAccess
readme =
  Signal.mailbox (MIDIAccess Dict.empty Dict.empty True)

report : MIDIAccess -> Task x ()
report markdown =
  Signal.send readme.address markdown


--main : Html
main =
  Signal.map show midiInPort
