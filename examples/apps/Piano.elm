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

import Music exposing (..)
import String exposing (..)
import List exposing (..)
import List.Extra exposing (takeWhile)
import Dict exposing (..)
-- MODEL

type KeyType = White | Black
defaultKeyLayout : List (KeyType, PitchClass)
defaultKeyLayout =  [(White, C), (Black, Cs), (White, D), (Black, Ds), (White, E), (White, F),
                     (Black, Fs), (White, G), (Black, Gs), (White, A), (Black, As), (White, B)]

defaultKeyLayoutLength : Int
defaultKeyLayoutLength = List.length defaultKeyLayout

type alias PianoKeyMap = (String, Pitch)

defaultMap1 : PianoKeyMap
defaultMap1 = ("q2w3er5t6y7uQ@W#ER%T^Y&U", (C,2))

defaultMap2 : PianoKeyMap
defaultMap2 = ("zsxdcvgbhnjmZSXDCVGBHNJM", (C,3))

defaultMap0 : PianoKeyMap
defaultMap0 = (fst defaultMap1 ++ fst defaultMap2, (C,3))

type alias Model =
    {
      firstOctave : Octave
    , width : Int
    , showLabels : Maybe PianoKeyMap
    }


init : (Model, Effects Action)
init =
  ( Model 1 2 (Just defaultMap1)
  , Effects.none
  )


-- UPDATE

type Action
  = None

update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
      None ->
        (model, Effects.none)

-- VIEW
(=>) : a -> b -> ( a, b )
(=>) = (,)

view : Signal.Address Action -> Model -> Html
view address model =
  div [ style [ "width" => "200px" ] ]
    [ h2 [] [text "Piano"]
    , keyboardHolder address model.firstOctave model.width model.showLabels
    ]

mkEvents octaves keyMap =
  Dict.empty

mkLbls octaves keyMap =
  case keyMap of
    Nothing ->
      List.repeat ((List.length octaves) * 12) ""
    Just (string, (pitch, oct )) ->
      let total = (List.length octaves)
          covered = (String.length string) // 12
          pre = takeWhile (\x -> x < oct) octaves
      in
        (List.repeat ((List.length pre) * 12) "")
        ++ (String.split "" string)
        ++ (List.repeat ((total-covered) * 12) "")

keyboardHolder : Address Action -> Octave -> Int -> Maybe PianoKeyMap -> Html
keyboardHolder address octave width keyMap =
  let octaves = [octave .. (octave + width)]
      labels = mkLbls octaves keyMap

      layout : Octave -> List (Octave, (KeyType, PitchClass))
      layout n = List.map2 (,) (List.repeat defaultKeyLayoutLength n) defaultKeyLayout
      keyboard = List.map layout octaves |> List.concat |> List.map2 (,) labels

      chooseKey (lb, (o, (kt, p))) (left, ks) =
        case kt of
          White -> (left + 40,  (whiteKey (p, o) lb left) :: ks )
          Black -> (left,       (blackKey (p, o) lb (left + 25 - 40)) :: ks )

      (totalWidth, keys) = List.foldl chooseKey (0, []) keyboard
  in
  div
  [ class "keyboard-holder"
  , style [ "width" => (toString totalWidth ++ "px") ]
  ]
  (List.reverse keys)


whiteKey : Pitch -> String -> Int -> Html
whiteKey p lbl left =
  div
  [ class "white key"
  , style [ "width" => "40px"
          , "height" => "200px"
          , "left" => (toString left ++ "px")
          ]
  ]
  [ div
    [ class "label" ]
    [ text lbl ]
  ]

blackKey : Pitch -> String -> Int -> Html
blackKey p lbl left =
  div
  [ class "black key"
  , style [ "width" => "30px"
          , "height" => "120px"
          , "left" => (toString left ++ "px")
          ]
  ]
  [ div
    [ class "label" ]
    [ text lbl ]
  ]
