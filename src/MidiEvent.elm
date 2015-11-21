module MidiEvent (..) where
{-| The datatypes for Midi Events

@docs Track, MEvent, ElapsedTime, MidiEvent, SystemEvent

@docs MPitch, Velocity, ControlNum, PBRange, ProgNum, Pressure, MidiChannel, ControlVal

-}

{-| MIDI File -}
type alias Track  = List MEvent


{-| MIDI Event -}
type MEvent = MidiEvent ElapsedTime MidiEvent
            | SystemEvent ElapsedTime SystemEvent
            | NoEvent

{-| MIDI ElapsedTime -}
type alias ElapsedTime  = Int


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

{-| System Events
Event emitted when a system MIDI message has been received.
-}
type SystemEvent = Sysex                    -- sysex
                 | Timecode                 -- timecode
                 | Songposition ElapsedTime -- songposition
                 | Songselect Int           -- songselect
                 | Tuningrequest            -- tuningrequest
                 | Sysexend                 -- sysexend
                 | Clock                    -- clock
                 | Start                    -- start
                 | Continue                 -- continue
                 | Stop                     -- stop
                 | Activesensing            -- activesensing
                 | Reset                    -- reset
                 | Unknown String
