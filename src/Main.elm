module Main exposing (main)

import Browser
import Constants
import Game
import Html
import Http
import Json.Decode exposing (Value)
import Tiled.Tiledmap as Tiledmap exposing (Tiledmap)



-- MODEL


type Model
    = Loading
    | Failure
    | Success Game.Model


init : Value -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Http.get
        { url = String.concat [ Constants.assetDir, "map.json" ]
        , expect = Http.expectJson LevelLoaded Tiledmap.decoder
        }
    )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "GGJ2020"
    , body =
        [ case model of
            Loading ->
                Html.text "Loading"

            Failure ->
                Html.text "Failed to Load"

            Success game ->
                Html.map GotGameMsg (Game.view game)
        ]
    }



-- UPDATE


type Msg
    = LevelLoaded (Result Http.Error Tiledmap)
    | GotGameMsg Game.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LevelLoaded result ->
            case result of
                Ok map ->
                    ( Success (Game.init map), Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )

        GotGameMsg gameMsg ->
            case model of
                Success game ->
                    let
                        ( newGameModel, newGameMsg ) =
                            Game.update gameMsg game
                    in
                    ( Success newGameModel, Cmd.map GotGameMsg newGameMsg )

                Loading ->
                    ( model, Cmd.none )

                Failure ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Success game ->
            Sub.map GotGameMsg (Game.subscriptions game)

        _ ->
            Sub.none



-- MAIN


main : Program Value Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
