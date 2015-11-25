module Perform (..) where
{-|
# This module convert a Music value into a value of type
Performance, which is an abstract notion of what the music means.

@docs Performance, Event, DurT, Context, Key

@docs metro, performM, merge, perf, toDelta, splitByInst, performToMidi, performToMEvs, mkMEvents,insertMEvent, division, toDelta

-}
import Music exposing (..)
import Time exposing (Time)
import MidiEvent exposing (..)

{-| Sequence of musical events -}
type alias Performance = List Event

{-| An event Event s i p d captures the fact that at start time s
 instrument i sounds pitch p for a duration d
-}
type alias Event =
  { eTime  : Time
  , eInst  : IName
  , ePitch : AbsPitch
  , eDur   : DurT
  }

{-| duration is measured in seconds, rather than beats -}
type alias DurT = Float

{-| Define the time to begin the performance, the default instrument
to use, and the proper key and tempo
-}
type alias Context =
  { cTime : Time                -- start time
  , cInst : IName               -- midi instrument
  , cDur  : DurT                -- note duration in sec
  , cKey  : Key                 -- composition Key
  }

{-| Performance Key -}
type alias Key = AbsPitch

{-| Given a standard beats per minute generates the duration of one
whole note.
 -}
metro : Float -> Dur -> DurT
metro setting dur = 60 / (setting * toFloatD dur)

{-| Model a "performer" as a function performM -}
performM : Context -> Music -> Performance
performM c m = fst (perf c m)

{-| efficient solution is to have perform compute the duration directly -}
perf : Context -> Music -> (Performance, DurT)
perf ({ cTime, cInst, cDur, cKey } as ctx) m =
  let transpose p k i =
        case i of
          Percussion -> absPitch p
          otherwise  -> absPitch p + k
  in
    case m of
      Note p d -> let d' = toFloatD d * cDur
                  in ([ Event cTime cInst (transpose p cKey cInst) d' ], d')
      Rest d ->  ([], toFloatD d * cDur)
      Sequence m1 m2 -> let (pf1,d1) = perf ctx m1
                            (pf2,d2) = perf {ctx | cTime <- cTime + d1} m2
                        in (pf1++pf2, d1+d2)
      Parallel m1 m2 -> let (pf1,d1) = perf ctx m1
                            (pf2,d2) = perf ctx m2
                        in (merge pf1 pf2, max d1 d2)
      Tempo  a  m -> perf {ctx | cDur <- cDur / toFloatD a} m
      Trans  p  m -> perf {ctx | cKey <- cKey + p} m
      Instr  nm m -> perf {ctx | cInst <- nm} m

{-| Merging two performances over time -}
merge : Performance -> Performance -> Performance
merge es1 es2 =
  case (es1, es2) of
    ([], es2) -> es2
    (es1, []) -> es1
    (e1::es1', e2::es2') ->
      if .eTime e1 < .eTime e2 then e1 :: merge es1' es2
      else e2 :: merge es1 es2'

{-|
there are 96 time-code divisions per quarter note
-}
division : Float
division = 96

{-| there are four times that many in a whole note; multiplying that
by the time-stamp on one of our Events gives us the proper delta-time
-}
toDelta : Float -> Float
toDelta t = (t * 4.0 * division)

{-| converts a Performance into the MidiFile data type -}
performToMidi : Performance -> MidiFile
performToMidi pf =
  MidiFile (Ticks division)
             (List.map performToMEvs (splitByInst pf))

{-|
we must associate each instrument with a separate track
it also assigns a unique channel number to each instrument (max 16)
-}
splitByInst : Performance -> List (MidiChannel,ProgNum,Performance)
splitByInst p =
  let
    aux n pf =
      case pf of
        [] -> []
        h :: tl ->
          let i = .eInst h
              (pf1,pf2) = List.partition (\e -> .eInst e == i) pf
              n'        = if n==8 then 10 else n+1
          in if i == Percussion
             then (9, 0, pf1) :: aux n pf2
             else (n, instrumentToInt i, pf1) :: aux n' pf2
  in aux 0 p

{-|
converts a Performance into a stream of MEvents (i.e., a Track).
-}
performToMEvs : (MidiChannel,ProgNum,Performance) -> Track
performToMEvs (ch,pn,perf)
  = let tempo = 500000
        setupInst   = ChannelEvent 0 (ProgChange ch pn)
        setTempo    = MetaEvent 0 (SetTempo tempo)
        loop p = case p of
                   [] -> []
                   e::es ->
                     let (mev1,mev2) = mkMEvents ch e
                     in  mev1 :: insertMEvent mev2 (loop es)
    in  -- setupInst ::
        setTempo :: loop perf

{-| Meke Note delimeters -}
mkMEvents : MidiChannel -> Event -> (MEvent,MEvent)
mkMEvents mChan { eTime, ePitch, eDur }
  = (ChannelEvent (toDelta eTime) (NoteOn  mChan ePitch 127),
     ChannelEvent (toDelta (eTime+eDur)) (NoteOff mChan ePitch 127))

{-| Insert event with respect of timestamp -}
insertMEvent : MEvent -> List MEvent -> List MEvent
insertMEvent (ChannelEvent t1 _ as ev)  evs =
  case evs of
    [] ->
      [ev]
    (ChannelEvent t2 _ as ev2) :: evs' ->
                               if t1 <= t2 then ev :: evs
                               else ev2 :: insertMEvent ev evs'
