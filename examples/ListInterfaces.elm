

import WebMidi exposing (..)
import Html exposing (..)
import Graphics.Element exposing (show)
import Dict exposing (Dict, empty, get)
import Task exposing (Task, andThen, succeed)

import Debug

synch = "1F01E2F5CAA14D73601F9D2DCD2521E51B4480AE41379B0116CE6970B0BC62A7"

midiOut : Signal.Mailbox MidiNote
midiOut =
  Signal.mailbox (MidiNote False 0)

port totalCapacity : Signal MidiNote
port totalCapacity = midiOut.signal

port midiAccess : Task x ()
port midiAccess =
  WebMidi.requestMIDIAccess defaultSettings
           `andThen` \midi -> let p = synch
                              in WebMidi.open synch totalCapacity
           `andThen` \p -> Signal.send midiOut.address (MidiNote True 42)
           `andThen` \out -> report midi


readme : Signal.Mailbox MIDIAccess
readme =
  Signal.mailbox (MIDIAccess Dict.empty Dict.empty True)

report : MIDIAccess -> Task x ()
report markdown =
  Signal.send readme.address markdown

--main : Html
main =
  Signal.map show readme.signal
