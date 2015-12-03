
import Html exposing (Html, text)
import Graphics.Element exposing (show, Element)
import Task exposing (Task, andThen, succeed)
import Maybe exposing (withDefault)

import WebMidi exposing (..)
import MidiEvent exposing (..)
import Music exposing (..)

synch = "Synth input port"

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

port midiAccess : Task String ()
port midiAccess =
  WebMidi.requestMIDIAccess defaultSettings
           `andThen` \midi -> Task.fromMaybe "No device found" (selectInstrument synch midi.outputs)
           `andThen` \id   -> WebMidi.enableOutput id midiOut.signal sysOut.signal
           `andThen` \p    -> Signal.send midiOut.address bell

bell = [ (1, (Control 1 0 0))     -- Control mode
       , (1, (ProgChange 1 14))   -- Set instrument
       , (1, (NoteOn 1 48 50))    -- Play C note
       , (1, (NoteOff 1 48 50))]  -- Stop note
     |> List.map encodeChannelEvent

main : Html
main = text "Play bell"
