

import Html exposing (Html)
import Graphics.Element exposing (show)
import Task exposing (Task, andThen, onError)
import Dict exposing (empty)
import Debug

import WebMidi exposing (..)


port midiAccess : Task x ()
port midiAccess = WebMidi.requestMIDIAccess defaultSettings
                  `andThen` report

midi : Signal.Mailbox MIDIAccess
midi =
  Signal.mailbox (MIDIAccess Dict.empty Dict.empty False)

report : MIDIAccess -> Task x ()
report midiAccess =
  Signal.send midi.address midiAccess

main =
  Signal.map show midi.signal
