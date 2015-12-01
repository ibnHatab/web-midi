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
    [ h2 [headerStyle] [text "Piano"]
    ]


headerStyle : Attribute
headerStyle =
  style
    [ "width" => "200px"
    , "text-align" => "center"
    ]
