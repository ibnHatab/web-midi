module IntervalAnimation where
{-| Animate mote cascade as in Synthesia
-}

import Color exposing (gray, red)
import Graphics.Element exposing (Element, flow, down, right, spacer, color)
import Animation exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

import Effects exposing (Effects)
import Time exposing (Time)
import Random

import Music exposing (..)

-- MODEL
type alias Model =
    { firstOctave : Octave
    , width       : Int
    , timeRef     : Float
    , animations : List Animation
    , clockTime : Time
    }

init : Octave -> Int -> (Model, Effects Action)
init oct width =
  ( Model oct width 0.0 animations 0
  , Effects.tick Tick
--  , Effects.none
  )

data : List Int
data =
    let gen = Random.int 1 12 |> Random.list 20
        seed = Random.initialSeed 42000
    in Random.generate gen seed |> fst

animations : List Animation
animations =
    List.scanl
        (\val prev -> prev |> to val |> Animation.delay (timeRemaining 0 prev))
        (static 0 |> speed 1.5)
        (List.map (\x -> toFloat x * 80) data) -- size up to bar length now for smoother animation



-- UPDATE
type Action
  = NoOp
  | Tick Time

update : Action -> Model -> (Model, Effects Action)
update action model =
    case action  |> Debug.log "act_anim"
    of
      NoOp ->
        ( model, Effects.none )

      Tick clockTime ->
      let _ = Debug.log "tick" clockTime
      in
        ( { model | clockTime = clockTime }
        , Effects.tick Tick
        )

-- VIEW
(=>) : a -> b -> ( a, b )
(=>) = (,)

view : Signal.Address Action -> Model -> Html
view address model =
  div [ style [ "width" => "200px" ] ]
    [ h2 [] [text "Interval Animation"]
      , fromElement <| render model.animations model.clockTime
    ]

render1 : Int -> Element
render1 x =
    flow down
        [ spacer 1 10
        , flow right
            [ spacer x 30 |> color gray
            , spacer (if x == 0 then 0 else 1) 30 |> color red
            ]
        ]

render : List Animation -> Time -> Element
render anims t =
    flow right
        [spacer 40 1
        , flow down <| List.map ((animate t)>>round>>render1) anims
        ]
