module PerformMusic where

import Html exposing (Html, text)
import Graphics.Element exposing (show, Element)

import Task exposing (Task, andThen, succeed)
import Maybe exposing (withDefault)
import Time exposing (second)

import WebMidi exposing (..)
import MidiEvent exposing (..)
import Music exposing (..)
import Perform exposing (..)

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

port midiAccess : Task String (List ())
port midiAccess =
  WebMidi.requestMIDIAccess defaultSettings
           `andThen` \midi  -> Task.fromMaybe "No device found" (selectInstrument synch midi.outputs)
           `andThen` \id    -> WebMidi.enableOutput id midiOut.signal sysOut.signal
           `andThen` \prt   -> Task.sleep second
           `andThen` \_     -> WebMidi.jiffy
           `andThen` \start -> List.map (play start) trackOne |> Task.sequence

play : Float -> MidiEvent -> Task a ()
play start (t, e) =
  Signal.send midiOut.address [encodeChannelEvent (t + start, e)]

-- SIMPLE TUNE
cMaj = [c,e',g] |> List.map (\n -> n 4 qn)

cMajArp = Music.line  cMaj
cMajChd = Music.chord cMaj

tune : Music
tune = (Music.repeatM 2 cMajArp) :+: cMajChd

-- MAKE PERFOMANCE
ctx : Context
ctx = Context 0 AcousticGrandPiano 2 0

ctx1: Context
ctx1= Context 0 TubularBells 2 0
bells = performM ctx1 (Music.repeatM 4 (g 4 wn))

performance : Performance
performance =
  performM ctx tune `merge`
  bells

midiFile = performance |> performToMidi

trackOne : List MidiEvent
trackOne =
  let (MidiFile (Ticks t) tracks) = midiFile
  in List.concat tracks

main : Element
main = show trackOne
