module MidiEvent (..) where
{-| The datatypes for Midi Events

The online MIDI 1.0 spec. http://www.midi.org/techspecs/midimessages.php

@docs MidiFile, Division, Track, MidiEvent, ElapsedTime, ChannelEvent, SystemEvent

@docs MPitch, Velocity, ControlNum, PBRange, ProgNum, Pressure, MidiChannel, ControlVal

@docs systemMessages, decodeSystemEvent, encodeSystemEvent

@docs channelMessages, decodeChannelEvent, encodeChannelEvent

# Meta and controll
@docs MetaEvent, MTempo,SMPTEHours,SMPTEMins,SMPTESecs,SMPTEFrames,SMPTEBits

-}

import WebMidi exposing (ChannelMessage, SystemMessage, ID, HighResTimeStamp)

{-| MIDI File -}
type MidiFile = MidiFile Division (List Track)

{-|-}
type Division = Ticks HighResTimeStamp

{-| Track -}
type alias Track  = List MidiEvent

{-| MIDI Event with timestamp -}
type alias MidiEvent = (ElapsedTime, ChannelEvent)

{-| MIDI ElapsedTime -}
type alias ElapsedTime  = HighResTimeStamp

{-| Midi Events
- NoteOn ch p v turns on note (pitch) p with velocity (volume) v on MIDI channel ch.
- NoteOff ch p v performs a similar function in turning the note off.
- The volume is an integer in the range 0 to 127.
- ProgChange ch pr sets the program number for channel ch to pr.
  This is how an instrument is selected.
-}
type ChannelEvent = NoteOff    MidiChannel MPitch Velocity       -- noteoff
                  | NoteOn     MidiChannel MPitch Velocity       -- noteon
                  | PolyAfter  MidiChannel MPitch Pressure       -- keyaftertouch
                  | ProgChange MidiChannel ProgNum               -- programchange
                  | Control    MidiChannel ControlNum ControlVal -- controlchange
                  | PitchBend  MidiChannel PBRange               -- pitchbend
                  | MonoAfter  MidiChannel Pressure              -- channelaftertouch
                  | Mode       MidiChannel ControlNum ControlVal -- channelmode
                  | UnknownChEv String

{-| System Events
 Event emitted when a system MIDI message has been received.
-}
type SystemEvent = Sysex ID                    -- sysex
                 | Timecode ID                 -- timecode
                 | Songposition ID Int         -- songposition
                 | Songselect ID Int           -- songselect
                 | Tuningrequest ID            -- tuningrequest
                 | Sysexend ID                 -- sysexend
                 | Clock ID                    -- clock
                 | Start ID                    -- start
                 | Continue ID                 -- continue
                 | Stop ID                     -- stop
                 | Activesensing ID            -- activesensing
                 | Reset ID                    -- reset
                 | UnknownSysEv String

{-|
Meta Events
-}
type MetaEvent = SequenceNum Int
               | TextEvent String
               | Copyright String
               | TrackName String
               | InstrName String
               | Lyric String
               | Marker String
               | CuePoint String
               | EndOfTrack
               | SetTempo MTempo
               | SMPTEOffset SMPTEHours SMPTEMins SMPTESecs SMPTEFrames SMPTEBits
               | TimeSig Int Int Int Int
               | SequencerSpecific List Int

{-| Type aliases for common values -}
type alias MPitch      = Int
{-|-}
type alias Velocity    = Int
{-|-}
type alias ControlNum  = Int
{-|-}
type alias PBRange     = Int
{-|-}
type alias ProgNum     = Int
{-|-}
type alias Pressure    = Int
{-|-}
type alias MidiChannel = Int
{-|-}
type alias ControlVal  = Int
{-|-}
type alias MTempo      = Int
{-|-}
type alias SMPTEHours  = Int
{-|-}
type alias SMPTEMins   = Int
{-|-}
type alias SMPTESecs   = Int
{-|-}
type alias SMPTEFrames = Int
{-|-}
type alias SMPTEBits   = Int


{-| Channel Messages commands -}
channelMessages : { noteoff: Int
                  , noteon: Int
                  , keyaftertouch: Int
                  , controlchange: Int
                  , channelmode: Int
                  , programchange: Int
                  , channelaftertouch: Int
                  , pitchbend: Int
                  }
channelMessages = { noteoff = 8            -- 0x8
                  , noteon = 9             -- 0x9
                  , keyaftertouch = 10     -- 0xA
                  , controlchange = 11     -- 0xB
                  , channelmode = 11       -- 0xB
                  , programchange = 12     -- 0xC
                  , channelaftertouch = 13 -- 0xD
                  , pitchbend = 14         -- 0xE
                  }

{-| Decode channel event and event time from received channel message
-}
decodeChannelEvent : ChannelMessage -> MidiEvent
decodeChannelEvent { command, data1, data2, timestamp, channel } =
  let channelEvent =
        if command == channelMessages.noteoff
             || (command == channelMessages.noteon && data2 == 0)
        then NoteOff channel data1 (data2 `rem` 127)

        else if command == channelMessages.noteon
        then NoteOn channel data1 (data2 `rem` 127)

        else if command == channelMessages.keyaftertouch
        then PolyAfter channel data1 (data2 `rem` 127)

        else if command == channelMessages.controlchange && data1 >= 0 && data1 <= 119
        then Control channel data1 data2

        else if command == channelMessages.channelmode && data1 >= 120 && data1 <= 127
        then Mode channel data1 data2

        else if command == channelMessages.programchange
        then ProgChange channel data1

        else if command == channelMessages.channelaftertouch
        then MonoAfter channel (data1 `rem` 127)

        else if command == channelMessages.pitchbend
        then PitchBend channel (((data2 * 128) + data1 - 8192) `rem` 8192)

        else UnknownChEv (toString command)
  in (timestamp, channelEvent)

{-| Encode MIDI Event into Channel event -}
encodeChannelEvent :  MidiEvent-> ChannelMessage
encodeChannelEvent (timestamp, event)  =
  let
    msgAt =
      case event of
        NoteOff ch pitch velocity       ->
          ChannelMessage channelMessages.noteoff pitch velocity ch
        NoteOn ch pitch velocity       ->
          ChannelMessage channelMessages.noteon pitch velocity ch
        PolyAfter ch pitch pressure    ->
          ChannelMessage channelMessages.keyaftertouch pressure -1 ch
        ProgChange ch num               ->
          ChannelMessage channelMessages.programchange num -1 ch
        Control ch num val ->
          ChannelMessage channelMessages.controlchange num val ch
        PitchBend ch range               ->
          ChannelMessage channelMessages.pitchbend ((range - 8192) `rem` 128) (range `rem` 128) ch
        MonoAfter ch pressure              ->
          ChannelMessage channelMessages.channelaftertouch pressure -1 ch
        Mode ch num val ->
          ChannelMessage channelMessages.channelmode num val ch
        otherwise -> ChannelMessage 0 0 0 0
  in msgAt timestamp

{-| System Messages event id -}
systemMessages : { activesensing : Int
                 , clock : Int
                 , continue : Int
                 , reset : Int
                 , songposition : Int
                 , songselect : Int
                 , start : Int
                 , stop : Int
                 , sysex : Int
                 , sysexend : Int
                 , timecode : Int
                 , tuningrequest : Int
                 , unknownsystemmessage : Int
                 }
systemMessages = { sysex = 240              -- 0xF0
                 , timecode = 241           -- 0xF1
                 , songposition = 242       -- 0xF2
                 , songselect = 243         -- 0xF3
                 , tuningrequest = 246      -- 0xF6
                 , sysexend = 247           -- 0xF7 (never actually received - simply ends a sysex)
                 , clock = 248              -- 0xF8
                 , start = 250              -- 0xFA
                 , continue = 251           -- 0xFB
                 , stop = 252               -- 0xFC
                 , activesensing = 254      -- 0xFE
                 , reset = 255              -- 0xFF
                 , unknownsystemmessage = -1
                 }

{-| Decode system event from system message -}
decodeSystemEvent : SystemMessage -> SystemEvent
decodeSystemEvent { event, device, data } =
  if event == systemMessages.sysex              then Sysex device
  else if event == systemMessages.timecode      then  Timecode device
  else if event == systemMessages.songposition  then  Songposition device data
  else if event == systemMessages.songselect    then  Songselect device data
  else if event == systemMessages.tuningrequest then  Tuningrequest device
  else if event == systemMessages.sysexend      then  Sysexend device
  else if event == systemMessages.clock         then  Clock device
  else if event == systemMessages.start         then  Start device
  else if event == systemMessages.continue      then  Continue device
  else if event == systemMessages.stop          then  Stop device
  else if event == systemMessages.activesensing then  Activesensing device
  else if event == systemMessages.reset         then  Reset device
  else UnknownSysEv ("Event: " ++ toString event ++ " on: " ++ device)

{-| Encode system messages -}
encodeSystemEvent : SystemEvent -> SystemMessage
encodeSystemEvent event =
  case event of
    Sysex dev            -> SystemMessage systemMessages.sysex dev -1
    Timecode dev         -> SystemMessage systemMessages.timecode dev -1
    Songposition dev val -> SystemMessage systemMessages.songposition dev val
    Songselect dev val   -> SystemMessage systemMessages.songselect dev val
    Tuningrequest dev    -> SystemMessage systemMessages.tuningrequest dev -1
    Sysexend dev         -> SystemMessage systemMessages.sysexend dev -1
    Clock dev            -> SystemMessage systemMessages.clock dev -1
    Start dev            -> SystemMessage systemMessages.start dev -1
    Continue dev         -> SystemMessage systemMessages.continue dev -1
    Stop dev             -> SystemMessage systemMessages.stop dev -1
    Activesensing dev    -> SystemMessage systemMessages.activesensing dev -1
    Reset dev            -> SystemMessage systemMessages.reset dev -1
    otherwise            -> SystemMessage systemMessages.unknownsystemmessage "" -1
