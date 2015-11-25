import String

import IO.IO exposing (..)
import IO.Runner exposing (Request, Response, run)
import ElmTest.Test exposing (test, suite, Test)
import ElmTest.Assertion exposing (assert, assertEqual)
import ElmTest.Runner.Console exposing (runDisplay)


import MidiEvent exposing (..)


tests : Test
tests = suite "MIDI Event decoder suite"
        [
         test "decode from kbd"
         ( let inputEvent =     -- C4 Off
                 {channel: 1
                 , command: 9
                 , data1: 48
                 , data2: 58
                 , timestamp: 1}
           in assertEqual (ChannelEvent 1 (NoteOff 1 48 0)) decodeChannelEvent inputEvent
         )

        ]

port requests : Signal Request
port requests = run responses (runDisplay tests)

port responses : Signal Response
