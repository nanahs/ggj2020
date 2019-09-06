module Main exposing (main)

import Browser exposing (Document)
import Html
import Html.Attributes as Attributes
import Json.Decode exposing (Value)
import Url exposing (Url)


type alias Model =
    { stuff : () }


init : Value -> ( Model, Cmd Msg )
init flags =
    ( { stuff = () }, Cmd.none )



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Tetris"
    , body =
        [ Html.div [ Attributes.class "text-4xl" ]
            [ Html.text "This is going to be tetris"
            ]
        ]
    }



-- UPDATE


type Msg
    = Ignored


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Ignored ->
            ( model, Cmd.none )


main : Program Value Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
