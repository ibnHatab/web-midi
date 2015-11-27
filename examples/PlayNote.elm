
import Html exposing (Html, text)
import Graphics.Element exposing (show, Element)
import Task exposing (Task, andThen, succeed)
import Maybe exposing (withDefault)

import WebMidi exposing (..)
import MidiEvent exposing (..)
import Music exposing (..)

synch = "Synth input port (16600:0)"

midiOut : Signal.Mailbox (List ChannelMessage)
midiOut =
  Signal.mailbox [none]

port midiOutPort : Signal (List ChannelMessage)
port midiOutPort = midiOut.signal

c4on = NoteOn 1 (absPitch (C, 4)) 50
f4on = NoteOn 1 (absPitch (F, 4)) 50
twoN = [ encodeChannelEvent 1000 c4on, encodeChannelEvent 2000 f4on]

port midiAccess : Task x ()
port midiAccess =
  WebMidi.requestMIDIAccess defaultSettings
           `andThen` \midi -> WebMidi.openM (withDefault "none"
                                             (selectInstrument synch midi.outputs)) midiOut.signal
           `andThen` \p -> Signal.send midiOut.address bell

bell = [ChannelEvent 1 (Control 1 0 0)
       ,ChannelEvent 1 (ProgChange 1 14)
       ,ChannelEvent 1 (NoteOn 1 48 50)
       ,ChannelEvent 1 (NoteOff 1 48 50)]
     |> List.map (\(ChannelEvent t e) -> encodeChannelEvent t e)

main : Html
main = text "Play bell"
