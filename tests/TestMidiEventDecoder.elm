import String

import IO.IO exposing (..)
import IO.Runner exposing (Request, Response, run)
import ElmTest.Test exposing (test, suite, Test)
import ElmTest.Assertion exposing (assert, assertEqual)
import ElmTest.Runner.Console exposing (runDisplay)

import Debug

import MidiEvent exposing (..)

inputEvent = { channel = 1
             , command = 8
             , data1 = 48
             , data2 = 0
             , timestamp = 1}

tests : Test
tests = suite "MIDI Event decoder suite"
        [
         test "decode from kbd"
         ( let a  = 1
           in assertEqual (ChannelEvent 1 (NoteOff 1 48 0)) (decodeChannelEvent inputEvent)
          )
        , test "recode"
         ( let (ChannelEvent timestamp che) = decodeChannelEvent inputEvent
           in assertEqual inputEvent ((encodeChannelEvent timestamp che) |> Debug.log "che")
         )


        ]

port requests : Signal Request
port requests = run responses (runDisplay tests)

port responses : Signal Response
