module WebMidi (..) where


{-| This module provides access to the Web MIDI API; the API is
designed to represent the low-level software protocol of MIDI, in
order to enable developers to build powerful MIDI software on top..

#basic access

@docs requestMIDIAccess, MIDIAccess, MIDIPort, Settings, defaultSettings

#sending and receiving MIDI events

@docs open, close, ID, MidiNote

-}

import Native.WebMidi
import Dict exposing (Dict, empty)
import Task exposing (Task, andThen, succeed)


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
  , pitch     : Int
  -- , velocity  : Int
  -- , timestamp : Int
  -- , sourceId  : String
  }

{-| Settings used by access to MIDI devices -}
type alias Settings =
    { sysex : Bool
    , onChange : Maybe (String -> Task () ())
    , midiNote : Maybe (Signal MidiNote)
    }

{-| The default settings used by access to MIDI devices -}
defaultSettings : Settings
defaultSettings =
    { sysex = False
    , onChange = Nothing
    , midiNote = Nothing
    }

{-| Obtaining Access to MIDI Devices -}
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
