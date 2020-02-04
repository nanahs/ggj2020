module Main exposing (main)

import Browser
import Browser.Events
import Constants
import Html exposing (Html)
import Html.Attributes as Attributes
import Http
import Json.Decode as Decode exposing (Decoder, Value)
import Player exposing (Player)
import Svg as Svg
import Svg.Attributes as SvgAttrs
import Tiled.Tiledmap as Map exposing (Tiledmap)
import Vector2 as Vector2 exposing (Vector2)


type alias Model =
    { isPause : Bool
    , player : Player
    , map : Maybe Tiledmap
    }


type Msg
    = Input KeyState Input
    | Tick Float
    | LevelLoaded (Result Http.Error Tiledmap)


type KeyState
    = Up
    | Down


type Input
    = MoveUp
    | MoveDown
    | MoveLeft
    | MoveRight
    | Pause


init : Value -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Http.get
        { url = String.concat [ Constants.assetDir, "map.json" ]
        , expect = Http.expectJson LevelLoaded Map.decoder
        }
    )


initialModel : Model
initialModel =
    { isPause = False
    , player = Player.initPlayer <| Vector2.create { x = 32, y = 32 }
    , map = Nothing
    }


clampY : Int -> Int -> Vector2 -> Vector2
clampY min max vect2 =
    if Vector2.getY vect2 < min then
        Vector2.create { x = Vector2.getX vect2, y = min }

    else if Vector2.getY vect2 > max then
        Vector2.create { x = Vector2.getX vect2, y = max }

    else
        vect2


clampX : Int -> Int -> Vector2 -> Vector2
clampX min max vect2 =
    if Vector2.getX vect2 < min then
        Vector2.create { x = min, y = Vector2.getY vect2 }

    else if Vector2.getX vect2 > max then
        Vector2.create { x = max, y = Vector2.getY vect2 }

    else
        vect2



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input keyState input ->
            Maybe.map
                (\map ->
                    let
                        player =
                            model.player

                        height =
                            Map.tileHeight map

                        width =
                            Map.tileWidth map

                        newPlayer =
                            { player
                                | pos =
                                    case ( input, keyState ) of
                                        ( MoveUp, Down ) ->
                                            Vector2.add player.pos (Vector2.create { x = 0, y = height * -1 })

                                        ( MoveDown, Down ) ->
                                            Vector2.add player.pos (Vector2.create { x = 0, y = height })

                                        ( MoveLeft, Down ) ->
                                            Vector2.add player.pos (Vector2.create { x = width * -1, y = 0 })

                                        ( MoveRight, Down ) ->
                                            Vector2.add player.pos (Vector2.create { x = width, y = 0 })

                                        _ ->
                                            player.pos
                            }
                    in
                    ( case input of
                        MoveUp ->
                            { model | player = newPlayer }

                        MoveDown ->
                            { model | player = newPlayer }

                        MoveLeft ->
                            { model | player = newPlayer }

                        MoveRight ->
                            { model | player = newPlayer }

                        Pause ->
                            { model | isPause = not model.isPause }
                    , Cmd.none
                    )
                )
                model.map
                |> Maybe.withDefault ( model, Cmd.none )

        Tick delta ->
            ( model, Cmd.none )

        LevelLoaded (Ok map) ->
            ( { model | map = Just map }, Cmd.none )

        LevelLoaded (Err e) ->
            ( model, Cmd.none )



-- VIEW


viewPlayer : Player -> Svg.Svg msg
viewPlayer player =
    Svg.image
        [ SvgAttrs.height <| String.fromInt 16
        , SvgAttrs.width <| String.fromInt 16
        , SvgAttrs.x <| String.fromInt <| Vector2.getX player.pos
        , SvgAttrs.y <| String.fromInt <| Vector2.getY player.pos
        , SvgAttrs.xlinkHref <| String.concat [ Constants.assetDir, "player.png" ]
        ]
        []


view : Model -> Tiledmap -> Html Msg
view model map =
    Html.div [ Attributes.class "flex justify-center h-screen bg-gray-700" ]
        [ Html.div
            [ Attributes.class "flex flex-col justify-center h-screen w-full"
            ]
            [ Svg.svg
                [ List.map String.fromInt [ 0, 0, Map.mapWidth map, Map.mapHeight map ]
                    |> String.join " "
                    |> SvgAttrs.viewBox
                , SvgAttrs.width "100%"
                , SvgAttrs.height "100%"
                ]
                [ Map.view map
                , viewPlayer model.player
                ]
            ]
        ]


main : Program Value Model Msg
main =
    Browser.document
        { init = init
        , view =
            \model ->
                { title = "GGJ2020"
                , body =
                    [ Maybe.map (view model) model.map
                        |> Maybe.withDefault (Html.text "No map loaded")
                    ]
                }
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown <|
            Decode.andThen (decodeInput Down) (Decode.field "key" Decode.string)
        , Browser.Events.onKeyUp <|
            Decode.andThen (decodeInput Up) (Decode.field "key" Decode.string)

        -- , Browser.Events.onAnimationFrameDelta (\f -> Tick <| f / 1000 )
        , Sub.none
        ]



-- Decoder


upDecoder : KeyState -> Decoder Msg
upDecoder keyState =
    Decode.succeed <| Input keyState MoveUp


downDecoder : KeyState -> Decoder Msg
downDecoder keyState =
    Decode.succeed <| Input keyState MoveDown


leftDecoder : KeyState -> Decoder Msg
leftDecoder keyState =
    Decode.succeed <| Input keyState MoveLeft


rightDecoder : KeyState -> Decoder Msg
rightDecoder keyState =
    Decode.succeed <| Input keyState MoveRight


decodeInput : KeyState -> String -> Decoder Msg
decodeInput keyState str =
    case str of
        "ArrowUp" ->
            upDecoder keyState

        "ArrowDown" ->
            downDecoder keyState

        "ArrowLeft" ->
            leftDecoder keyState

        "ArrowRight" ->
            rightDecoder keyState

        "w" ->
            upDecoder keyState

        "s" ->
            downDecoder keyState

        "a" ->
            leftDecoder keyState

        "d" ->
            rightDecoder keyState

        "Escape" ->
            case keyState of
                Up ->
                    Decode.succeed <| Input Up Pause

                _ ->
                    Decode.fail ""

        _ ->
            Decode.fail ""
