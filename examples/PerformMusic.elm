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
           `andThen` \prt -> WebMidi.jiffy
           `andThen` \start -> List.map (play start) track |> Task.sequence


play start e =
  case e of
    ChannelEvent t chev ->
      Signal.send midiOut.address (encodeChannelEvent (t+start) chev)
    otherwise ->
      succeed ()

cMaj = [c,e',g] |> List.map (\n -> n 4 hn)

cMajArp = Music.line  cMaj
cMajChd = Music.chord cMaj

ctx = Context 0 AcousticGrandPiano 1 0

file = performM ctx ((Music.repeatM 3 cMajArp) :+: cMajChd)
     |> performToMidi

track =
  let
    (MidiFile (Ticks t) tracks) = file
  in List.head tracks |> withDefault []

main = show track
