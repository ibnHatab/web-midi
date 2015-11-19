module Music where
{-| This module provides basic musical concepts.

# The Music Data Type
@docs Pitch, PitchClass, Octave

# Composition
@docs Music, Dur, IName, trans

# Arifmetic
@docs Ratio, normalizeR, addR, multiplyR, toFloatR
@docs (:+:), (:=:), (!!), (:%:)

# Representation
@docs AbsPitch, absPitch, pitches, pitch, pcToInt


@docs NoteDecorator,cf,c, cs, df, d, ds, ef, e, es
@docs ff, f, fs, gf, g, gs, af, a, as', bf, b, bs

-}

{- This module provides basic musical concepts.


-}

import Array exposing (Array, get)

{-| Define how high or low a note is
-}
type alias Pitch = (PitchClass, Octave)

{-| Pitch class (i.e., one of 12 semitones)
-}
type PitchClass = Cf | C | Cs | Df | D | Ds | Ef | E | Es | Ff | F
                | Fs | Gf | G | Gs | Af | A | As | Bf | B | Bs

{-| The interval between one musical pitch and another with half or
double its frequency
-}
type alias Octave = Int

{-| Ratio data type -}
type Ratio = Ratio Int Int
{-| Normalize Ration post arophmetic -}
normalizeR : Ratio -> Ratio
normalizeR (Ratio p q) =
  let
    gcd : Int -> Int -> Int
    gcd a b = if b == 0 then a else gcd b (a % b)
    k = gcd p q * (if q < 0 then -1 else 1)
  in Ratio (p // k) (q // k)
{-| Ration addition -}
addR : Ratio -> Ratio -> Ratio
addR (Ratio a b) (Ratio c d) =
  normalizeR (Ratio (a * d + b * c) (b * d))
{-| Ratio multiplication  -}
multiplyR : Ratio -> Ratio -> Ratio
multiplyR (Ratio a b) (Ratio c d) =
  normalizeR (Ratio (a * c) (b * d))
{-| Radio to Float -}
toFloatR : Ratio -> Float
toFloatR (Ratio a b) = Basics.toFloat a / Basics.toFloat b



{-| Musical structures are captured in Music data type
 - m1 :+: m2 is the “sequential composition” of m1 and m2 (i.e., m1 and m2 are played in sequence).
 - m1 :=: m2 is the “parallel composition” of m1 and m2 (i.e., m1 and m2 are played simultaneously).
 - Tempo a m scales the rate at which m is played (i.e., its tempo) by a factor of a.
 - Trans i m transposes m by interval i (in semitones).
 - Instr iname m declares that m is to be performed using instrument iname
-}
type Music = Note Pitch Dur
          | Rest Dur
          | Sequence Music Music
          | Parallel Music Music
          | Tempo  (Ratio Int) Music
          | Trans  Int Music
          | Instr  IName Music

{-| the “sequential composition” -}
(:+:) : Music -> Music -> Music
(:+:) = Sequence

{-| the “parallel composition” -}
(:=:) : Music -> Music -> Music
(:=:) = Parallel

{-| Durations and tempo scalings are represented using rational numbers -}
type alias Dur = Ratio Int

{-| General MIDI Standard instruments -}
type IName
  = AcousticGrandPiano  | BrightAcousticPiano | ElectricGrandPiano
  | HonkyTonkPiano      | RhodesPiano         | ChorusedPiano
  | Harpsichord   | Clavinet        | Celesta | Glockenspiel  | MusicBox
  | Vibraphone | Marimba  | Xylophone           | TubularBells
  | Dulcimer              | HammondOrgan        | PercussiveOrgan
  | RockOrgan | ChurchOrgan         | ReedOrgan
  | Accordion             | Harmonica           | TangoAccordion
  | AcousticGuitarNylon   | AcousticGuitarSteel | ElectricGuitarJazz
  | ElectricGuitarClean   | ElectricGuitarMuted | OverdrivenGuitar
  | DistortionGuitar      | GuitarHarmonics     | AcousticBass
  | ElectricBassFingered  | ElectricBassPicked  | FretlessBass
  | SlapBass1             | SlapBass2           | SynthBass1 | SynthBass2
  | Violin        | Viola | Cello  | Contrabass | TremoloStrings
  | PizzicatoStrings      | OrchestralHarp      | Timpani
  | StringEnsemble1       | StringEnsemble2     | SynthStrings1
  | SynthStrings2         | ChoirAahs           | VoiceOohs | SynthVoice
  | OrchestraHit          | Trumpet             | Trombone  | Tuba
  | MutedTrumpet          | FrenchHorn          | BrassSection | SynthBrass1
  | SynthBrass2           | SopranoSax          | AltoSax | TenorSax
  | BaritoneSax    | Oboe | Bassoon  | EnglishHorn          | Clarinet
  | Piccolo               | Flute    | Recorder | PanFlute  | BlownBottle
  | Shakuhachi            | Whistle  | Ocarina  | Lead1Square
  | Lead2Sawtooth         | Lead3Calliope       | Lead4Chiff
  | Lead5Charang          | Lead6Voice          | Lead7Fifths
  | Lead8BassLead         | Pad1NewAge          | Pad2Warm
  | Pad3Polysynth         | Pad4Choir           | Pad5Bowed
  | Pad6Metallic          | Pad7Halo            | Pad8Sweep
  | FX1Train              | FX2Soundtrack       | FX3Crystal
  | FX4Atmosphere         | FX5Brightness       | FX6Goblins
  | FX7Echoes             | FX8SciFi            | Sitar | Banjo  | Shamisen
  | Koto | Kalimba        | Bagpipe             | Fiddle | Shanai
  | TinkleBell    | Agogo | SteelDrums          | Woodblock      | TaikoDrum
  | MelodicDrum           | SynthDrum           | ReverseCymbal
  | GuitarFretNoise       | BreathNoise         | Seashore
  | BirdTweet             | TelephoneRing       | Helicopter
  | Applause              | Gunshot             | Percussion

{-| Treating pitches simply as integers
-}
type alias AbsPitch = Int

{-| Converter AbsPitch from Pitch -}
absPitch : Pitch -> AbsPitch
absPitch (pc,oct) = 12*oct + pcToInt pc

{-| Pitches LUT -}
pitches : Array PitchClass
pitches = Array.fromList [C,Cs,D,Ds,E,F,Fs,G,Gs,A,As,B]

infixl 9 !!
{-| Array indexer -}
(!!) : Array a -> Int -> a
(!!) array i = case Array.get i array of Just a -> a

{-| Pitch of AbsPitch -}
pitch : AbsPitch -> Pitch
pitch ap = ( pitches !! (ap % 12),
             ap // 12 )

{-| Pitches Dict -}
pcToInt : PitchClass -> Int
pcToInt pc = case pc of
               Cf -> -1   -- should Cf be 11?
               C  -> 0
               Cs -> 1
               Df -> 1
               D  -> 2
               Ds -> 3
               Ef -> 3
               E  -> 4
               Es -> 5
               Ff -> 4
               F  -> 5
               Fs -> 6
               Gf -> 6
               G  -> 7
               Gs -> 8
               Af -> 8
               A  -> 9
               As -> 10
               Bf -> 10
               B  -> 11
               Bs -> 12 -- should Bs be 0?

{-| transposes pitches -}
trans : Int -> Pitch -> Pitch
trans i p = pitch (absPitch p + i)

-- cf,c,cs,df,d,ds,ef,e,es,ff,f,fs,gf,g,gs,af,a,as,bf,b,bs
--   : Octave -> Dur -> Music

{-|-}
type alias NoteDecorator = Octave -> Dur -> Music

{-|-}
cf : NoteDecorator
cf o = Note (Cf,o)
{-|-}
c : NoteDecorator
c o = Note (C,o)

{-|-}
cs : NoteDecorator
cs o = Note (Cs,o)
{-|-}
df : NoteDecorator
df o = Note (Df,o)
{-|-}
d : NoteDecorator
d  o = Note (D,o)
{-|-}
ds : NoteDecorator
ds o = Note (Ds,o)
{-|-}
ef : NoteDecorator
ef o = Note (Ef,o)
{-|-}
e : NoteDecorator
e  o = Note (E,o)
{-|-}
es : NoteDecorator
es o = Note (Es,o)
{-|-}
ff : NoteDecorator
ff o = Note (Ff,o)
{-|-}
f : NoteDecorator
f  o = Note (F,o)
{-|-}
fs : NoteDecorator
fs o = Note (Fs,o)
{-|-}
gf : NoteDecorator
gf o = Note (Gf,o)
{-|-}
g : NoteDecorator
g  o = Note (G,o)
{-|-}
gs : NoteDecorator
gs o = Note (Gs,o)
{-|-}
af : NoteDecorator
af o = Note (Af,o)
{-|-}
a : NoteDecorator
a  o = Note (A,o)
{-|-}
as' : NoteDecorator
as' o = Note (As,o)
{-|-}
bf : NoteDecorator
bf o = Note (Bf,o)
{-|-}
b : NoteDecorator
b  o = Note (B,o)
{-|-}
bs : NoteDecorator
bs o = Note (Bs,o)

{-| Ratio shorthand -}
(:%:) : Int -> Int -> Ratio
(:%:) = Ratio

-- wn  = 1:%:1
-- wnr  = Rest wn      -- whole

-- hn  = 1 :%: 2
-- hnr  = Rest hn      -- half

-- qn  = 1 :%: 4
-- qnr  = Rest qn      -- quarter
-- en  = 1 :%: 8
-- enr  = Rest en      -- eight
-- sn  = 1 :%: 16
-- snr  = Rest sn      -- sixteenth
-- tn  = 1 :%: 32
-- tnr  = Rest tn      -- thirty-second
-- dhn = 3 :%: 4
-- dhnr = Rest dhn     -- dotted half
-- dqn = 3 :%: 8
-- dqnr = Rest dqn     -- dotted quarter
-- den = 3 :%: 16
-- denr = Rest den     -- dotted eighth
-- dsn = 3 :%: 32

-- dsnr = Rest dsn     -- dotted sixteenth
