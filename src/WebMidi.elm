module WebMidi (..) where

{-| This module provides access to the Web MIDI API; the API is
designed to represent the low-level software protocol of MIDI, in
order to enable developers to build powerful MIDI software on top..

# Basic access to
@docs requestMIDIAccess, Settings, MIDIAccess, MIDIPort


@docs defaultSettings

# Wiring MIDI I/O devices with runtime signals.

@docs enableInput, enableOutput, close, ID, selectInstrument

# Sends a MIDI message to the specified device(s) at the specified
  timestamp.

@docs ChannelMessage, initChannelMsg, initSystemMsg, SystemMessage, HighResTimeStamp


# Utils to synchronously perform music

@docs jiffy, channel, system, onChange

-}
import Native.WebMidi
import Dict exposing (Dict, empty)
import Task exposing (Task, andThen, succeed)
import Regex exposing (contains, regex)

{-| Device ID -}
type alias ID = String

{-| This interface represents a MIDI input or output port.  -}
type alias MIDIPort = {
    name         : String
  , manufacturer : String
  , version      : String
  }

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
initChannelMsg : ChannelMessage
initChannelMsg = ChannelMessage 0 0 0 0 0

{-| -}
type alias SystemMessage =
  { event  : Int
  , device : ID
  , data   : Int
  }

{-| Not a message -}
initSystemMsg : SystemMessage
initSystemMsg = SystemMessage 0 "" 0

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

{-| Open Output MIDI Devices -}
enableOutput : ID -> Signal (List ChannelMessage) -> Signal SystemMessage -> Task x ID
enableOutput =
  Native.WebMidi.enableOutput

{-| Open Input MIDI Devices -}
enableInput : ID -> Task x ID
enableInput =
  Native.WebMidi.enableInput


{-| Close MIDI Devices -}
close : ID -> Task x ID
close =
  Native.WebMidi.close

{-| Select device  ID by device name -}
selectInstrument : String -> Dict ID MIDIPort -> Maybe ID
selectInstrument name instruments =
  let ids = Dict.foldr (\key val keyList ->
                        if contains (regex name) val.name then key :: keyList
                        else keyList) [] instruments
  in List.head ids


{-| High Resolution Time used in MIDI events. -}
jiffy : Task x Float
jiffy =
  Native.WebMidi.jiffy

{-| channel input multiplexer -}
channel : Signal ChannelMessage
channel =
  Native.WebMidi.channel

{-| system input multiplexer -}
system : Signal SystemMessage
system =
  Native.WebMidi.system

{-| on change notification -}
onChange : Signal ID
onChange =
  Native.WebMidi.onChange


{-
playNote : Address ChannelMessage -> ChannelMessage
 -> Task x ()
playNote addr note =
  Signal.send addr note

stopNote

sendControlChange

sendSystemMessage

-}
