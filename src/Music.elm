module Music where
{-| This module provides basic musical concepts.

# The Music Data Type
@docs Pitch, PitchClass, Octave, Mode

# Composition
@docs Music, Dur, IName, trans, dur

# Arifmetic
@docs normalizeD, addD, multiplyD, divideD, toFloatD, maxD
@docs (:+:), (:=:), (!!), (:%:)

# Representation
@docs AbsPitch, absPitch, pitches, pitch, pcToInt

@docs cf,c, cs, df, d, ds, ef, e', es
@docs ff, f, fs, gf, g, gs, af, a, as', bf, b, bs

# Duration
@docs wn,  hn,  qn,  en,  sn,  tn
@docs dhn, dqn, den, dsn, zero

# Music
@docs wnr, hnr, qnr, enr, snr, tnr
@docs dhnr, dqnr, denr, dsnr

# Higher-Level Constructions
@docs line, chord, delay, repeatM

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

{-| The Key Signature specifies a mode, either major or minor. -}
type Mode = Major | Minor

{-| Durations and tempo scalings are represented using rational numbers -}
type Dur = Dur Int Int

{-| Normalize Durn post arophmetic -}
normalizeD : Dur -> Dur
normalizeD (Dur p q) =
  let
    gcd : Int -> Int -> Int
    gcd a b = if b == 0 then a else gcd b (a % b)
    k = gcd p q * (if q < 0 then -1 else 1)
  in Dur (p // k) (q // k)

{-| Durn addition -}
addD : Dur -> Dur -> Dur
addD (Dur a b) (Dur c d) =
  normalizeD (Dur (a * d + b * c) (b * d))

{-| Dur multiplication  -}
multiplyD : Dur -> Dur -> Dur
multiplyD (Dur a b) (Dur c d) =
  normalizeD (Dur (a * c) (b * d))

{-| Dur division -}
divideD : Dur -> Dur -> Dur
divideD r (Dur a b) =
  multiplyD r (Dur b a)

{-| Radio to Float -}
toFloatD : Dur -> Float
toFloatD (Dur a b) = Basics.toFloat a / Basics.toFloat b

{-| Max duration -}
maxD : Dur -> Dur -> Dur
maxD d1 d2 =
  if toFloatD d1 > toFloatD d2 then d1 else d2

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
           | Tempo  Dur Music
           | Trans  Int Music
           | Instr  IName Music

{-| the “sequential composition” -}
(:+:) : Music -> Music -> Music
(:+:) = Sequence

{-| the “parallel composition” -}
(:=:) : Music -> Music -> Music
(:=:) = Parallel


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
cf : Octave -> Dur -> Music
cf o = Note (Cf,o)
{-|-}
c : Octave -> Dur -> Music
c o = Note (C,o)

{-|-}
cs : Octave -> Dur -> Music
cs o = Note (Cs,o)
{-|-}
df : Octave -> Dur -> Music
df o = Note (Df,o)
{-|-}
d : Octave -> Dur -> Music
d  o = Note (D,o)
{-|-}
ds : Octave -> Dur -> Music
ds o = Note (Ds,o)
{-|-}
ef : Octave -> Dur -> Music
ef o = Note (Ef,o)
{-|-}
e' : Octave -> Dur -> Music
e'  o = Note (E,o)
{-|-}
es : Octave -> Dur -> Music
es o = Note (Es,o)
{-|-}
ff : Octave -> Dur -> Music
ff o = Note (Ff,o)
{-|-}
f : Octave -> Dur -> Music
f  o = Note (F,o)
{-|-}
fs : Octave -> Dur -> Music
fs o = Note (Fs,o)
{-|-}
gf : Octave -> Dur -> Music
gf o = Note (Gf,o)
{-|-}
g : Octave -> Dur -> Music
g  o = Note (G,o)
{-|-}
gs : Octave -> Dur -> Music
gs o = Note (Gs,o)
{-|-}
af : Octave -> Dur -> Music
af o = Note (Af,o)
{-|-}
a : Octave -> Dur -> Music
a  o = Note (A,o)
{-|-}
as' : Octave -> Dur -> Music
as' o = Note (As,o)
{-|-}
bf : Octave -> Dur -> Music
bf o = Note (Bf,o)
{-|-}
b : Octave -> Dur -> Music
b  o = Note (B,o)
{-|-}
bs : Octave -> Dur -> Music
bs o = Note (Bs,o)

{-| Dur shorthand -}
(:%:) : Int -> Int -> Dur
(:%:) = Dur

{-| zero delay -}
zero : Dur
zero = 0 :%: 1

{-| whale -}
wn : Dur
wn  = 1:%:1
{-| half -}
hn : Dur
hn  = 1 :%: 2
{-| quarter -}
qn : Dur
qn  = 1 :%: 4
{-| eight -}
en : Dur
en  = 1 :%: 8
{-| sixteenth -}
sn : Dur
sn  = 1 :%: 16
{-| thirty-second -}
tn : Dur
tn  = 1 :%: 32
{-| dotted half -}
dhn : Dur
dhn = 3 :%: 4
{-| dotted quarter -}
dqn : Dur
dqn = 3 :%: 8
{-| dotted eight -}
den : Dur
den = 3 :%: 16
{-| dotted sixteenth -}
dsn : Dur
dsn = 3 :%: 32

{-| whole -}
wnr : Music
wnr  = Rest wn
{-| half -}
hnr : Music
hnr  = Rest hn
{-| quarter -}
qnr : Music
qnr  = Rest qn
{-| eight -}
enr : Music
enr  = Rest en
{-| sixteenth -}
snr : Music
snr  = Rest sn
{-| thirty-second -}
tnr : Music
tnr  = Rest tn
{-| dotted half -}
dhnr : Music
dhnr = Rest dhn
{-| dotted quarter -}
dqnr : Music
dqnr = Rest dqn
{-| dotted eighth -}
denr : Music
denr = Rest den
{-| dotted sixteenth -}
dsnr : Music
dsnr = Rest dsn

{-| a line or melody -}
line : List Music -> Music
line  = List.foldr (:+:) (Rest zero)

{-| chord -}
chord : List Music -> Music
chord = List.foldr (:=:) (Rest zero)

{-| delay -}
delay : Dur -> Music -> Music
delay d m = Rest d :+: m

{-| repeat music -}
repeatM : Int -> Music -> Music
repeatM n m =  List.repeat n m |> line

{-| duration in beats of a musical structure -}
dur : Music -> Dur
dur music =
  case music of
    (Note _ d)    -> d
    (Rest d)      -> d
    (Sequence m1 m2)   -> dur m1 `addD` dur m2
    (Parallel m1 m2)   -> dur m1 `maxD` dur m2
    (Tempo  a  m) -> dur m `divideD` a
    (Trans  _  m) -> dur m
    (Instr  _  m) -> dur m
