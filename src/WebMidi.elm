module WebMidi (..) where

{-| This module provides access to the Web MIDI API; the API is
designed to represent the low-level software protocol of MIDI, in
order to enable developers to build powerful MIDI software on top..

# Basic access to
@docs requestMIDIAccess, Settings, MIDIAccess, MIDIPort, inputs


@docs defaultSettings

# Wiring MIDI I/O devices with runtime signals.

@docs open, close, ID

# Sends a MIDI message to the specified device(s) at the specified
  timestamp.

@docs MidiNote, none


# Utils to synchronously perform music

@docs performance


-}

import Native.WebMidi
import Dict exposing (Dict, empty)
import Task exposing (Task, andThen, succeed)

import MidiEvent exposing (..)


{-| This interface represents a MIDI input or output port.  -}
type alias MIDIPort = {
    name         : String
  , manufacturer : String
  , version      : String
  }

{-| Device ID -}
type alias ID = String

{-| This interface provides the methods to list MIDI input and output
devices, and obtain access to an individual device.  -}
type alias MIDIAccess = {
    inputs : Dict ID  MIDIPort
  , outputs : Dict ID MIDIPort
  , sysexEnabled : Bool
  }

{-| MIDI event -}
type alias MidiNote =
  { noteOn    : Bool
  , pitch     : (Int, Int)
  , velocity  : Int
  , timestamp : Int
  , channel   : Int
  }

{-| Not a Note
-}
none : MidiNote
none = MidiNote False (0,0) 0 0 0

{-| Settings used by access to MIDI devices and informing on
configuration changes. -}
type alias Settings =
  { sysex : Bool
  , onChange : Maybe (String -> Task () ())
  }

{-| The default settings used by access to MIDI devices -}
defaultSettings : Settings
defaultSettings =
    { sysex = False
    , onChange = Nothing
--    , midiNote = Nothing
    }

{-| Checks if the Web MIDI API is available and then tries to connect
to the host's MIDI subsystem.
-}
requestMIDIAccess : Settings -> Task x MIDIAccess
requestMIDIAccess =
  Native.WebMidi.requestMIDIAccess

{-| Open MIDI Devices -}
open : ID -> Signal MidiNote -> Task x MIDIPort
open =
  Native.WebMidi.open

{-| Close MIDI Devices -}
close : ID -> Task x MIDIPort
close =
  Native.WebMidi.close

{-| The current performance time used in MIDI events. -}
performance : Signal float
performance =
  Native.WebMidi.performance

{-| input multiplexer -}
inputs : Signal MidiNote
inputs =
  Native.WebMidi.inputs


{-
playNote : Address MidiNote -> MidiNote -> Task x ()
playNote addr note =
  Signal.send addr note

stopNote

sendControlChange

sendSystemMessage

-}
