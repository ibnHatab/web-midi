

import Html exposing (..)
import Graphics.Element exposing (show)

import WebMidi
import Signal
import Mouse
import Time


perf = Signal.sampleOn (Time.every Time.second) WebMidi.performance

main = Signal.map show perf
       -- Mouse.position


--
