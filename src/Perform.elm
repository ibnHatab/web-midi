module Perform (..) where
{-|
# This module convert a Music value into a value of type
Performance, which is an abstract notion of what the music means.

@docs Performance, Event, DurT, Context, Key

@docs metro, performM, merge, perf

-}
import Music exposing (..)
import Time exposing (Time)

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
  { cTime : Time
  , cInst : IName
  , cDur  : DurT
  , cKey  : Key
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
merge (e1::es1 as a) (e2::es2 as b) =
  case (es1, es2) of
    ([], res) ->
      res
    (les, []) ->
      les
    otherwise ->
      if .eTime e1 < .eTime e2 then e1 :: merge es1 b
      else e2 :: merge a es2
