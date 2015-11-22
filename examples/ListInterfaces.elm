


import Html exposing (..)
import Graphics.Element exposing (show)
import Dict exposing (Dict, empty, get)
import Task exposing (Task, andThen, succeed)
import Maybe exposing (withDefault)
import Debug

import WebMidi exposing (..)
import MidiEvent exposing (..)
import Music exposing (..)

synch = "Synth input port (16600:0)"
keyboard = "Virtual Keyboard"

selectInstrument : String -> Dict ID MIDIPort -> ID
selectInstrument name instruments =
  let ids = Dict.foldr (\key val keyList ->
                        if val.name == name then key :: keyList
                        else keyList) [] instruments
  in List.head ids |> withDefault "unknown"

midiOut : Signal.Mailbox ChannelMessage
midiOut =
  Signal.mailbox none

port midiOutPort : Signal ChannelMessage
port midiOutPort = midiOut.signal

c4on = NoteOn 1 (absPitch (C, 4)) 50

port midiAccess : Task x ()
port midiAccess =
  WebMidi.requestMIDIAccess defaultSettings
           `andThen` \midi -> WebMidi.open (selectInstrument synch midi.outputs) midiOut.signal
           `andThen` \outPort -> WebMidi.open (selectInstrument keyboard midi.inputs) WebMidi.channel
           `andThen` \p -> Signal.send midiOut.address (encodeChannelEvent c4on 0)
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
  Signal.map show (Signal.map2 (,) WebMidi.channel WebMidi.system)
