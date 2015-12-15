import String

-- import IO.IO exposing (..)
-- import IO.Runner exposing (Request, Response, run)

-- import ElmTest.Test exposing (test, suite, Test)
-- import ElmTest.Assertion exposing (assert, assertEqual)
-- import ElmTest.Runner.Console exposing (runDisplay)

import Console
import ElmTest exposing (..)
import String
import Task
import Debug

import MidiEvent exposing (..)
import Music exposing (..)

inputEvent = { channel = 1
             , command = 8
             , data1 = 48
             , data2 = 0
             , timestamp = 1}

tests : Test
tests = suite "MIDI Event decoder suite"
        [
        --  test "decode from kbd"
        --  ( let a  = 1
        --    in assertEqual (MidiEvent 1 (NoteOff 1 48 0)) (decodeChannelEvent inputEvent)
        --   )
        -- , test "recode"
        --  ( let (MidiEvent timestamp che) = decodeChannelEvent inputEvent
        --    in assertEqual inputEvent ((encodeChannelEvent timestamp che) |> Debug.log "che")
        --  )
        -- ,
        test "encode symetry pitch"
         (
          let freq = absPitch (C, 4)
          in assertEqual (C, 4) (pitch freq)
         )
        ,
        test "decode raw message C4 pressed"
        (
        let (t, ev) = (1,NoteOn 1 60 0)
        in  assertEqual (C, 4) (pitch 60)
        )

        ]

port runner : Signal (Task.Task x ())
port runner =
    Console.run (consoleRunner tests)

-- port requests : Signal Request
-- port requests = run responses (runDisplay tests)

-- port responses : Signal Response
