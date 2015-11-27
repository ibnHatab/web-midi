
import Html exposing (Html, text)
import Task exposing (Task, andThen, succeed)
import Maybe exposing (withDefault)

import WebMidi exposing (..)
import MidiEvent exposing (..)
import Music exposing (..)

synch = "Synth input port (16600:0)"

midiOut : Signal.Mailbox (List ChannelMessage)
midiOut =
  Signal.mailbox [initChannelMsg]
port midiOutPort : Signal (List ChannelMessage)
port midiOutPort = midiOut.signal

sysOut : Signal.Mailbox SystemMessage
sysOut =
  Signal.mailbox initSystemMsg
port sysOutPort : Signal SystemMessage
port sysOutPort = sysOut.signal

c4on = NoteOn 1 (absPitch (C, 4)) 50

port midiAccess : Task String ()
port midiAccess =
  WebMidi.requestMIDIAccess defaultSettings
           `andThen` \midi -> Task.fromMaybe "No device found" (selectInstrument synch midi.outputs)
           `andThen` \id   -> WebMidi.enableOutput id midiOut.signal sysOut.signal
           `andThen` \p -> Signal.send midiOut.address [encodeChannelEvent 0 c4on]

main : Html
main = text "Play C4"
