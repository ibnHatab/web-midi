module WebMidi (..) where

{-| This module provides access to the Web MIDI API; the API is
designed to represent the low-level software protocol of MIDI, in
order to enable developers to build powerful MIDI software on top..

# Basic access to
@docs requestMIDIAccess, Settings, MIDIAccess, MIDIPort


@docs defaultSettings

# Wiring MIDI I/O devices with runtime signals.

@docs open, close, ID

# Sends a MIDI message to the specified device(s) at the specified
  timestamp.

@docs ChannelMessage, none, SystemMessage, HighResTimeStamp


# Utils to synchronously perform music

@docs performance, channel, system


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

{-| milliseconds accurate to 5 microseconds -}
type alias HighResTimeStamp = Float

{-| ChannelMessage objects implementing MIDIInput interface -}
type alias ChannelMessage =
  { command   : Int
  , data1     : Int
  , data2     : Int
  , channel   : Int
  , timestamp : HighResTimeStamp
  }

{-| Not a message -}
none : ChannelMessage
none = ChannelMessage 0 0 0 0 0

{-| -}
type alias SystemMessage =
  { event  : Int
  , device : ID
  , data   : Int
  }

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
open : ID -> Signal ChannelMessage -> Task x MIDIPort
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

{-| channel input multiplexer -}
channel : Signal ChannelMessage
channel =
  Native.WebMidi.channel

{-| system input multiplexer -}
system : Signal ChannelMessage
system =
  Native.WebMidi.system

{-
playNote : Address ChannelMessage -> ChannelMessage
 -> Task x ()
playNote addr note =
  Signal.send addr note

stopNote

sendControlChange

sendSystemMessage

-}
