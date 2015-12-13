module Piano where
{-|
http://www.keithwhor.com/music/
-}

import Effects exposing (Effects)
import Signal exposing (Signal, Address)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import String as S exposing (length, split)
import List as L exposing (map, length, repeat)
import List.Extra as LX exposing (takeWhile, zip)
import Dict exposing (..)

import Char exposing (KeyCode)
import Set exposing (Set)

import WebMidi exposing (..)
import MidiEvent exposing (..)
import Music exposing (..)

-- MODEL
type KeyType = White | Black

{-| PC keyboard layout to pitch map -}
type alias PianoKeyMap = (String, Pitch)

{-| Blueprint to draw a keyboard -}
type alias KeyboardSpec = List ( String, ( Octave, ( KeyType, PitchClass ) ) )

type alias Model =
    { firstOctave : Octave
    , width       : Int
    , showLabels  : Maybe PianoKeyMap
    , keyboard    : KeyboardSpec
    , pressedKeys : List Pitch
    , mouseOn     : List Pitch
    , modiOutput  : Signal.Address (List ChannelMessage)
    , timeRef     : Float
    }

init : Octave -> Int -> Maybe PianoKeyMap
     -> Signal.Address (List ChannelMessage)
     -> (Model, Effects Action)
init octave width keymap modiOutput =
  let keyboard = mkKeyboarSpec octave width keymap
  in
    ( { firstOctave = octave
      , width = width
      , showLabels = keymap
      , keyboard = keyboard
      , pressedKeys = []
      , mouseOn = []
      , modiOutput = modiOutput
      , timeRef = 0
    }
    , Effects.map TimeRef (Effects.task WebMidi.jiffy)
    )

bwKeyLayout : List (KeyType, PitchClass)
bwKeyLayout =  [(White, C), (Black, Cs), (White, D), (Black, Ds), (White, E), (White, F),
                (Black, Fs), (White, G), (Black, Gs), (White, A), (Black, As), (White, B)]

mkLbls : List Octave -> Maybe PianoKeyMap -> List String
mkLbls octaves keyMap =
  case keyMap of
    Nothing ->
      repeat ((L.length octaves) * 12) ""
    Just (string, (pitch, oct )) ->
      let total = (L.length octaves)
          covered = (S.length string) // 12
          pre = LX.takeWhile (\x -> x < oct) octaves
      in
        (repeat ((L.length pre) * 12) "")
        ++ (split "" string)
        ++ (L.repeat ((total-covered) * 12) "")

mkKeyboarSpec : Octave -> Int -> Maybe PianoKeyMap -> KeyboardSpec
mkKeyboarSpec octave width keyMap =
  let octaves = [octave .. (octave + width - 1)]
      labels = mkLbls (octaves |> Debug.log "0") keyMap

      layout : Octave -> List (Octave, (KeyType, PitchClass))
      layout n = LX.zip (repeat (L.length bwKeyLayout) n) bwKeyLayout
   in
     L.map layout octaves |> L.concat |> LX.zip labels

defaultMap1 : PianoKeyMap
defaultMap1 = ("q2w3er5t6y7uQ@W#ER%T^Y&U", (C,2))

defaultMap2 : PianoKeyMap
defaultMap2 = ("zsxdcvgbhnjmZSXDCVGBHNJM", (C,3))

defaultMap0 : PianoKeyMap
defaultMap0 = (fst defaultMap1 ++ fst defaultMap2, (C,2))

expandKeyMap : PianoKeyMap -> Dict Char Pitch
expandKeyMap (string, (pitch, oct )) =
    let covered = (S.length string) // 12
        class = List.concat <|
                List.repeat covered [C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B]
        octave = List.concat <|
                 List.map (List.repeat 12) [oct .. (oct+covered)]
        pitches = List.map2 (,) class octave
    in
      List.map2 (,) (S.toList string) pitches |> Dict.fromList

codeSpetialKeys = [('`', '~'), ('1', '!'), ('2', '@'), ('3', '#'), ('4', '$'),
                   ('5', '%'), ('6', '^'), ('7', '&'), ('8', '*'), ('9', '('),
                   ('0', ')'), ('-', '_'), ('=', '+'), ('[', '{'), (']', '}'),
                   ('|', '\\'), ('\'', '\"'), (';', ':'), ('/', '?'), ('.', '>'),
                   (',', '<')] |> Dict.fromList

fromCodeSpetial : Int -> Char
fromCodeSpetial code =
  let c = Char.fromCode code
  in Maybe.withDefault c (Dict.get c codeSpetialKeys)

keyName : PitchClass -> String
keyName p = case p of
              C  -> "C"
              Cs -> "#C"
              D  -> "D"
              Ds -> "#D"
              E  -> "E"
              F  -> "F"
              Fs -> "#F"
              G  -> "G"
              Gs -> "#G"
              A  -> "A"
              As -> "#A"
              B  -> "B"
              otherwise -> ""

-- UPDATE
type Action
  = None
  | PithOn  (List Pitch)
  | PithOff (List Pitch)
  | MouseOn Pitch
  | MouseOff Pitch
  | TimeRef Float
  | NoOp


sendToMidi ctor address pitches =
  let play p = encodeChannelEvent (0, ctor 1 (absPitch p) 90)
      events = List.map play pitches
  in
  Signal.send address events |> Effects.task |> Effects.map (\_ -> NoOp)

update : Action -> Model -> (Model, Effects Action)
update action model =
    case action -- |> Debug.log "act_piano"
    of
      None ->
        (model, Effects.none)
      PithOn pitches ->
        ( { model | pressedKeys = pitches ++ model.pressedKeys }
        , sendToMidi NoteOn model.modiOutput pitches
        )
      PithOff pitches ->
        let (stop, rest) = L.partition (\p -> (L.member p pitches)) model.pressedKeys
        in
        ( { model | pressedKeys = rest }
        , sendToMidi NoteOff model.modiOutput stop
        )
      MouseOn p ->
        ( { model | mouseOn = [p] }
        , sendToMidi NoteOn model.modiOutput [p]
        )
      MouseOff p ->
        ( { model | mouseOn = [] }
        , sendToMidi NoteOff model.modiOutput [p]
        )
      TimeRef t ->
        ( { model | timeRef = t }
        , Effects.none
        )
      NoOp ->
        ( model, Effects.none )


-- VIEW
(=>) : a -> b -> ( a, b )
(=>) = (,)

view : Signal.Address Action -> Model -> Html
view address model =
  div [ style [ "width" => "200px" ] ]
    [ h2 [] [text "Piano"]
    , keyboardHolder address model.keyboard (model.mouseOn ++ model.pressedKeys)
    ]

keyboardHolder : Address Action -> KeyboardSpec -> List Pitch -> Html
keyboardHolder address keyboard pressed =
  let
      chooseKey (lb, (o, (kt, p))) (left, ks) =
        case kt of
          White -> (left + 40, (whiteKey address (L.member (p, o) pressed)
                                           (p, o) lb left) :: ks )
          Black -> (left,      (blackKey address (L.member (p, o) pressed)
                                           (p, o) lb (left + 25 - 40)) :: ks )

      (totalWidth, keys) = L.foldl chooseKey (0, []) keyboard
  in
  div
  [ class "keyboard-holder"
  , style [ "width" => (toString totalWidth ++ "px") ]
  ]
  (L.reverse keys)


whiteKey : Address Action -> Bool -> Pitch -> String -> Int -> Html
whiteKey address pressed p lbl left =
  div
  [ class "white key"
  , style ([ "width" => "40px"
           , "height" => "200px"
           , "left" => (toString left ++ "px")
           ]
           ++ (if pressed
               then [ "margin-top" => "5px"
                    , "box-shadow" => "none"
                    , "background-color" => "rgb(255, 0, 0)"]
               else []))
  , onMouseDown address (MouseOn p)
  , onMouseUp address (MouseOff p)
  ]
  [ div
    [ class "label" ]
    [ text lbl, br[][], br[][], text ((keyName (fst p)) ++ (toString (snd p))) ]
  ]

blackKey : Address Action -> Bool -> Pitch -> String -> Int -> Html
blackKey address pressed p lbl left =
  div
  [ class "black key"
  , style ([ "width" => "30px"
           , "height" => "120px"
           , "left" => (toString left ++ "px")
           ]
           ++ (if pressed
               then [ "margin-top" => "5px"
                    , "box-shadow" => "none"
                    , "background-color" => "rgb(255, 0, 0)"]
               else []))
  , onMouseDown address (MouseOn p)
  , onMouseUp address (MouseOff p)
  ]
  [ div
    [ class "label" ]
    [ text lbl, br[][], br[][], text ((keyName (fst p)) ++ (toString (snd p))) ]
  ]
