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
    }


init : (Model, Effects Action)
init =
  ( Model 2 2
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
    , keyboardHolder address model.firstOctave model.width
    ]

-- <div id="keyboard" class="keyboard-holder" style="width: 840px;">
keyboardHolder : Address Action -> Octave -> Int -> Html
keyboardHolder address octave width =
  let octaves = [octave .. (octave + width)]
      layout : Octave -> List (Octave, (KeyType, PitchClass))
      layout n = List.map2 (,) (List.repeat defaultKeyLayoutLength n) defaultKeyLayout
      keyboard = List.map layout octaves |> List.concat

      chooseKey (o, (kt, p)) (left, ks) =
        case kt of
          White -> (left + 40,  (whiteKey (p, o) left) :: ks )
          Black -> (left,       (blackKey (p, o) (left + 25 - 40)) :: ks )

      (totalWidth, keys) = List.foldl chooseKey (0, []) keyboard
  in
  div
  [ class "keyboard-holder"
  , style [ "width" => (toString totalWidth ++ "px") ]
  ]
  (List.reverse keys)

-- <div class="white key" id="KEY_C,-1" style="width: 40px; height: 200px; left: 0px; margin-top: 5px; box-shadow: none; background-color: rgb(255, 0, 0);">
-- <div class="label"><b>Q</b><br><br>C<span name="OCTAVE_LABEL" value="-1">3</span></div></div>

whiteKey : Pitch -> Int -> Html
whiteKey p left =
  div
  [ class "white key"
  , style [ "width" => "40px"
          , "height" => "200px"
          , "left" => (toString left ++ "px")
          ]
  ]
  [ div
    [ class "key-label" ]
    [ span [] [] ]
  ]

blackKey : Pitch -> Int -> Html
blackKey p left =
  div
  [ class "black key"
  , style [ "width" => "30px"
          , "height" => "120px"
          , "left" => (toString left ++ "px")
          ]
  ]
  [ div
    [ class "key-label" ]
    [ span [] [] ]
  ]
