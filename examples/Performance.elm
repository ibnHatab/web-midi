

import Html exposing (..)
import Graphics.Element exposing (show)

import WebMidi
import Signal
import Mouse
import Time
import Music exposing (..)


perf = Signal.sampleOn (Time.every Time.second) Mouse.position

cMaj = [c,e',g] |> List.map (\n -> n 4 qn)

cMajArp = Music.line  cMaj
cMajChd = Music.chord cMaj

-- perfect fifth
fifth m = m :=: Trans 7 m

mkLn n p d = line <| List.repeat n (Note p d)


-- pr1 : Pitch -> Music
-- pr1 p = Tempo (5%6)
--         (Tempo (4%3) (mkLn 1 p qn :+:
--                       Tempo (3%2) (mkLn 3 p en :+:
--                                    mkLn 2 p sn :+:
--                                    mkLn 1 p qn    ) :+:
--                       mkLn 1 p qn) :+:
--          Tempo (3%2) (mkLn 6 p en))

-- pr2 : Pitch -> Music
-- pr2 p =
--   let m1 = Tempo (5%4) (Tempo (3%2) m2 :+: m2)
--       m2 = mkLn 3 p
--   in
--     Tempo (7%6)
--             (m1 :+:
--                   Tempo (5%4) (mkLn 5 p en) :+:
--              m1 :+:
--                   Tempo (3%2) m2)


main = show cMajChd

-- Signal.map show perf
-- Mouse.position


--
