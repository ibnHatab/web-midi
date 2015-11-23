
import Html exposing (Html, text)
import Task exposing (Task, andThen, succeed)
import Maybe exposing (withDefault)

import WebMidi exposing (..)
import MidiEvent exposing (..)
import Music exposing (..)

synch = "Synth input port (16600:0)"

midiOut : Signal.Mailbox ChannelMessage
midiOut =
  Signal.mailbox none

port midiOutPort : Signal ChannelMessage
port midiOutPort = midiOut.signal

c4on = NoteOn 1 (absPitch (C, 4)) 50

port midiAccess : Task String ()
port midiAccess =
  WebMidi.requestMIDIAccess defaultSettings
           `andThen` \midi -> Task.fromMaybe "No device found" (selectInstrument synch midi.outputs)
           `andThen` \id -> WebMidi.open id midiOut.signal
           `andThen` \prt -> Signal.send midiOut.address (encodeChannelEvent c4on 0)

main : Html
main = text "Play C4"
