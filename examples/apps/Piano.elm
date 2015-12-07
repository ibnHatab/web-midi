module Piano where
{-|
http://www.keithwhor.com/music/
-}

import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (style)


-- MODEL

type KeyType = White1 | White2 | White3 | Black1

type alias Model =
    {
    }


init : (Model, Effects Action)
init =
  ( Model
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

(=>) = (,)

view : Signal.Address Action -> Model -> Html
view address model =
  div [ style [ "width" => "200px" ] ]
    [ h2 [] [text "Piano"]
    ]


-- <div id="keyboard" class="keyboard-holder" style="width: 840px;">

-- <div class="white key" id="KEY_C,-1" style="width: 40px; height: 200px; left: 0px; margin-top: 5px; box-shadow: none; background-color: rgb(255, 0, 0);"><div class="label"><b>Q</b><br><br>C<span name="OCTAVE_LABEL" value="-1">3</span></div></div>



-- <div class="black key" id="KEY_C#,-1" style="width: 30px; height: 120px; left: 25px;"><div class="label"><b>2</b><br><br>C<span name="OCTAVE_LABEL" value="-1">3</span>#</div></div>
