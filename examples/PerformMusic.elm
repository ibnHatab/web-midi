module PerformMusic where

import Html exposing (Html, text)
import Graphics.Element exposing (show)

import Task exposing (Task, andThen, succeed)
import Maybe exposing (withDefault)
import Time exposing (..)
import WebMidi exposing (..)
import MidiEvent exposing (..)
import Music exposing (..)
import Perform exposing (..)

synch = "Synth input port (16600:0)"

midiOut : Signal.Mailbox ChannelMessage
midiOut =
  Signal.mailbox none

port midiOutPort : Signal ChannelMessage
port midiOutPort = midiOut.signal

c4on = NoteOn 1 (absPitch (C, 4)) 50

port midiAccess : Task String (List ())
port midiAccess =
  WebMidi.requestMIDIAccess defaultSettings
           `andThen` \midi -> Task.fromMaybe "No device found" (selectInstrument synch midi.outputs)
           `andThen` \id -> WebMidi.open id midiOut.signal
           `andThen` \prt -> List.map play track |> Task.sequence

--             Signal.send midiOut.address (encodeChannelEvent c4on 0)

play e =
  case e of
    ChannelEvent t chev ->
      Signal.send midiOut.address (encodeChannelEvent chev 0)
    otherwise ->
      succeed ()


-- Signal.send midiOut.address (encodeChannelEvent c4on 0)

cMaj = [c,e',g] |> List.map (\n -> n 4 qn)

cMajArp = Music.line  cMaj
cMajChd = Music.chord cMaj

ctx = Context 0 AcousticGrandPiano 1 (absPitch (C, 4))

file = performM ctx cMajChd
     |> performToMidi

track =
  let
    (MidiFile (Ticks t) tracks) = file
  in List.head tracks |> withDefault []

-- scheduler : Signal Time
-- scheduler = Time.every second

-- serializer = Signal.foldp select (Nothing, track) scheduler

-- select t tracks =
--   case tracks of
--     (_, []) -> (Nothing, [])
--     (_, s :: rest) -> (Just s, rest)

-- serialize : Signal.Mailbox ChannelMessage -> MidiFile -> Signal Time
-- serialize mb (MidiFile (Ticks tick) traks) =
--   let batch t tracks = List.map \track ->
--                        List.takeWhile \tr ->
--   in Signal.foldp send tracks


-- midiPlayer : Signal (Task x ())
-- midiPlayer = Signal.map play serializer

-- main : Html
-- main = Signal.map (show) serializer
main = show track
--  text "Play"
