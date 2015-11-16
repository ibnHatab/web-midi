module WebMidi (..) where


{-| This module provides access to the Web MIDI API; the API is
designed to represent the low-level software protocol of MIDI, in
order to enable developers to build powerful MIDI software on top..

#basic access

@docs requestMIDIAccess, MIDIAccess, MIDIPort

-}

import Native.WebMidi
import Dict exposing (Dict, empty)


{-| This interface represents a MIDI input or output port.  -}
type alias MIDIPort = {
    id : String
  , manufacturer : String
  , name : String
  }

{-| This interface provides the methods to list MIDI input and output
devices, and obtain access to an individual device.  -}
type alias MIDIAccess = {
    inputs : Dict String  MIDIPort
  , outputs : Dict String MIDIPort
  , sysexEnabled : Bool
  }

{-| Obtaining Access to MIDI Devices -}
requestMIDIAccess : Bool -> MIDIAccess
requestMIDIAccess sysex = MIDIAccess Dict.empty Dict.empty True
