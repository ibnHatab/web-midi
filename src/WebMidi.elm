module WebMidi (..) where


{-| This module provides access to the Web MIDI API; the API is
designed to represent the low-level software protocol of MIDI, in
order to enable developers to build powerful MIDI software on top..

#basic access

@docs requestMIDIAccess, MIDIAccess, MIDIPort

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

{-| This interface provides the methods to list MIDI input and output
devices, and obtain access to an individual device.  -}
type alias MIDIAccess = {
    inputs : Dict String  MIDIPort
  , outputs : Dict String MIDIPort
  , sysexEnabled : Bool
  }

type alias Settings =
    { sysex : Bool
    , onChange : Maybe (Task () ())
    , midiNote : Maybe (Signal MidiNote)
    }

{-| The default settings used by MIDIaccess -}
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
