
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

port midiAccess : Task x ()
port midiAccess =
  WebMidi.requestMIDIAccess defaultSettings
           `andThen` \midi -> WebMidi.open (withDefault "none"
                                            (selectInstrument synch midi.outputs)) midiOut.signal
           `andThen` \p -> Signal.send midiOut.address (encodeChannelEvent 0 c4on)

main : Html
main = text "Play C4"
