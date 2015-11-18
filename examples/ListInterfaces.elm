

import WebMidi exposing (..)
import Html exposing (..)
import Graphics.Element exposing (show)
import Dict exposing (Dict, empty)
import Task exposing (Task, andThen, succeed)

import Debug

port midiAccess : Task x ()
port midiAccess =
  WebMidi.requestMIDIAccess True `andThen` report

readme : Signal.Mailbox MIDIAccess
readme =
  Signal.mailbox (MIDIAccess Dict.empty Dict.empty True)

report : MIDIAccess -> Task x ()
report markdown =
  Signal.send readme.address markdown

--main : Html
main =
  Signal.map show readme.signal
