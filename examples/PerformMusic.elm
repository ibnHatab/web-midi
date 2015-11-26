module PerformMusic where

import Html exposing (Html, text)
import Graphics.Element exposing (show, Element)

import Task exposing (Task, andThen, succeed)
import Maybe exposing (withDefault)

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

port midiAccess : Task String (List ())
port midiAccess =
  WebMidi.requestMIDIAccess defaultSettings
           `andThen` \midi  -> Task.fromMaybe "No device found" (selectInstrument synch midi.outputs)
           `andThen` \id    -> WebMidi.open id midiOut.signal
           `andThen` \prt   -> WebMidi.jiffy
           `andThen` \start -> List.map (play start) trackOne |> Task.sequence

play : Float -> MEvent -> Task a ()
play start e =
  case e of
    ChannelEvent t chev ->
      Signal.send midiOut.address (encodeChannelEvent (t+start) chev)
    otherwise ->
      succeed ()

-- SIMPLE TUNE
cMaj = [c,e',g] |> List.map (\n -> n 4 hn)

cMajArp = Music.line  cMaj
cMajChd = Music.chord cMaj

tune : Music
tune = (Music.repeatM 3 cMajArp) :+: cMajChd

-- POLYRITHM
pr1 : Pitch -> Music
pr1 p = Tempo (5:%:6)
        (Tempo (4:%:3) (mkLn 1 p qn :+:
                        Tempo (3:%:2) (mkLn 3 p en :+:
                                       mkLn 2 p sn :+:
                                       mkLn 1 p qn ) :+:
                        mkLn 1 p qn) :+:
         Tempo (3:%:2) (mkLn 6 p en))

pr2 : Pitch -> Music
pr2 p =
  let m1 = Tempo (5:%:4) (Tempo (3:%:2) m2 :+: m2)
      m2 = mkLn 3 p en
  in Tempo (7:%:6)
       (m1 :+:
        Tempo (5:%:4) (mkLn 5 p en) :+:
        m1 :+:
        Tempo (3:%:2) m2)

mkLn n p d = repeatM n (Note p d)

poly : Music
poly = pr1 (C,5) :=: pr2 (G,5)

-- MAKE PERFOMANCE
ctx : Context
ctx = Context 0 AcousticGrandPiano 3 0

performance : Performance
performance = performM ctx tune

trackOne : List MEvent
trackOne =
  let (MidiFile (Ticks t) tracks) = performance |> performToMidi
  in List.head tracks |> withDefault []

main : Element
main = show trackOne
