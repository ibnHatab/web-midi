module MidiEvent (..) where
{-| The datatypes for Midi Events

The online MIDI 1.0 spec. http://www.midi.org/techspecs/midimessages.php

@docs MidiFile, Division, Track, MEvent, ElapsedTime, MidiEvent, SystemEvent

@docs MPitch, Velocity, ControlNum, PBRange, ProgNum, Pressure, MidiChannel, ControlVal

@docs systemMessages, decodeSystemEvent, encodeSystemEvent

@docs channelMessages, decodeChannelEvent, encodeChannelEvent

-}

import WebMidi exposing (ChannelMessage, SystemMessage, ID, HighResTimeStamp)

{-| MIDI File -}
type MidiFile = MidiFile Division (List Track)

{-|-}
type Division = Ticks Int
{-| Track -}
type alias Track  = List MEvent

{-| MIDI Event -}
type MEvent = ChannelEvent ElapsedTime MidiEvent
            | SystemEvent ElapsedTime SystemEvent
            | NoEvent

{-| MIDI ElapsedTime -}
type alias ElapsedTime  = HighResTimeStamp

{-|-}
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

{-| Midi Events
- NoteOn ch p v turns on note (pitch) p with velocity (volume) v on MIDI channel ch.
- NoteOff ch p v performs a similar function in turning the note off.
- The volume is an integer in the range 0 to 127.
- ProgChange ch pr sets the program number for channel ch to pr.
  This is how an instrument is selected.
-}
type MidiEvent = NoteOff    MidiChannel MPitch Velocity       -- noteoff
               | NoteOn     MidiChannel MPitch Velocity       -- noteon
               | PolyAfter  MidiChannel MPitch Pressure       -- keyaftertouch
               | ProgChange MidiChannel ProgNum               -- programchange
               | Control    MidiChannel ControlNum ControlVal -- controlchange
               | PitchBend  MidiChannel PBRange               -- pitchbend
               | MonoAfter  MidiChannel Pressure              -- channelaftertouch
               | Mode       MidiChannel ControlNum ControlVal -- channelmode


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
decodeChannelEvent : ChannelMessage -> MEvent
decodeChannelEvent { command, data1, data2, timestamp, channel } =
  let midiEvent =
        if | command == channelMessages.noteoff || (command == channelMessages.noteon && data2 == 0)
             -> NoteOff channel data1 (data2 // 127)
           | command == channelMessages.noteon
             -> NoteOn channel data1 (data2 // 127)
           | command == channelMessages.keyaftertouch
             -> PolyAfter channel data1 (data2 // 127)
           | command == channelMessages.controlchange && data1 >= 0 && data1 <= 119
             -> Control channel data1 data2
           | command == channelMessages.channelmode && data1 >= 120 && data1 <= 127
             -> Mode channel data1 data2
           | command == channelMessages.programchange
             -> ProgChange channel data1
           | command == channelMessages.channelaftertouch
             -> MonoAfter channel (data1 // 127)
           | command == channelMessages.pitchbend
             -> PitchBend channel (((data2 * 128) + data1 - 8192) // 8192)
  in ChannelEvent timestamp midiEvent

{-| Encode MIDI Event into Channel event -}
encodeChannelEvent : MidiEvent -> ElapsedTime -> ChannelMessage
encodeChannelEvent event timestamp =
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
          ChannelMessage channelMessages.pitchbend ((range - 8192) // 128) (range // 128) ch
        MonoAfter ch pressure              ->
          ChannelMessage channelMessages.channelaftertouch pressure -1 ch
        Mode ch num val ->
          ChannelMessage channelMessages.channelmode num val ch
  in msgAt timestamp


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
                 | Unknown String

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
systemMessages ={ sysex = 240              -- 0xF0
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
  if | event == systemMessages.sysex         ->  Sysex device
     | event == systemMessages.timecode      ->  Timecode device
     | event == systemMessages.songposition  ->  Songposition device data
     | event == systemMessages.songselect    ->  Songselect device data
     | event == systemMessages.tuningrequest ->  Tuningrequest device
     | event == systemMessages.sysexend      ->  Sysexend device
     | event == systemMessages.clock         ->  Clock device
     | event == systemMessages.start         ->  Start device
     | event == systemMessages.continue      ->  Continue device
     | event == systemMessages.stop          ->  Stop device
     | event == systemMessages.activesensing ->  Activesensing device
     | event == systemMessages.reset         ->  Reset device
     | event == systemMessages.unknownsystemmessage
       ->  Unknown ("Event: " ++ toString event ++ " on: " ++ device)

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
