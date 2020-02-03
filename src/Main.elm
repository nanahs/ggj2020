module Main exposing (main)

import Browser
import Browser.Events
import Constants
import Entity exposing (Entity)
import Html exposing (Html)
import Html.Attributes as Attributes
import Http
import Json.Decode as Decode exposing (Decoder, Value)
import Player exposing (Player)
import Svg as Svg
import Svg.Attributes as SvgAttrs
import Tile exposing (Tile)
import Tiledmap exposing (Tiledmap)
import Vector2 as Vector2 exposing (Vector2)


type alias Model =
    { isPause : Bool
    , player : Player
    , map : Maybe Tiledmap
    }


type Msg
    = Input KeyState Input
    | Tick Float
    | PauseGame
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
        , expect = Http.expectJson LevelLoaded Tiledmap.decoder
        }
    )


initialModel : Model
initialModel =
    { isPause = False
    , player = Player.initPlayer <| Vector2.create { x = 32, y = 32 }
    , map = Nothing
    }


clampY : Float -> Float -> Vector2 -> Vector2
clampY min max vect2 =
    if Vector2.getY vect2 < min then
        Vector2.create { x = Vector2.getX vect2, y = min }

    else if Vector2.getY vect2 > max then
        Vector2.create { x = Vector2.getX vect2, y = max }

    else
        vect2


clampX : Float -> Float -> Vector2 -> Vector2
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
                            Tiledmap.tileHeight map

                        width =
                            Tiledmap.tileWidth map

                        newPlayer =
                            { player
                                | pos =
                                    case ( input, keyState ) of
                                        ( MoveUp, Down ) ->
                                            Vector2.add player.pos (Vector2.create { x = 0, y = toFloat <| height * -1 })

                                        ( MoveDown, Down ) ->
                                            Vector2.add player.pos (Vector2.create { x = 0, y = toFloat height })

                                        ( MoveLeft, Down ) ->
                                            Vector2.add player.pos (Vector2.create { x = toFloat <| width * -1, y = 0 })

                                        ( MoveRight, Down ) ->
                                            Vector2.add player.pos (Vector2.create { x = toFloat width, y = 0 })

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

        -- Tick delta ->
        --     ( { model
        --         | player =
        --             doJump model.jump model.player
        --                 |> doMove model.left model.right
        --                 |> Entity.updatePos delta Vector2.zero gameBoard
        --       }
        --     , Cmd.none
        --     )
        Tick delta ->
            ( model, Cmd.none )

        PauseGame ->
            ( model, Cmd.none )

        LevelLoaded (Ok map) ->
            ( { model | map = Just map }, Cmd.none )

        LevelLoaded (Err e) ->
            ( model, Cmd.none )



-- VIEW


blackAttr : Svg.Attribute msg
blackAttr =
    SvgAttrs.class "text-gray-700 fill-current"


grayAttr : Svg.Attribute msg
grayAttr =
    SvgAttrs.class "text-gray-300 fill-current"


redAttr : Svg.Attribute msg
redAttr =
    SvgAttrs.class "text-red-500 fill-current"


viewPlayer : Player -> Svg.Svg msg
viewPlayer player =
    Svg.image
        [ SvgAttrs.height <| String.fromInt 16
        , SvgAttrs.width <| String.fromInt 16
        , SvgAttrs.x <| String.fromFloat <| Vector2.getX player.pos
        , SvgAttrs.y <| String.fromFloat <| Vector2.getY player.pos
        , SvgAttrs.xlinkHref <| String.concat [ Constants.assetDir, "player.png" ]
        ]
        []


view : Model -> Html Msg
view model =
    Html.div [ Attributes.class "flex justify-center h-screen bg-gray-700" ]
        [ Html.div
            [ Attributes.class "flex flex-col justify-center h-screen w-full"
            ]
            [ model.map
                |> Maybe.map
                    (\map ->
                        Svg.svg
                            [ List.map String.fromInt [ 0, 0, Tiledmap.mapWidth map, Tiledmap.mapHeight map ]
                                |> String.join " "
                                |> SvgAttrs.viewBox
                            , SvgAttrs.width "100%"
                            , SvgAttrs.height "100%"
                            ]
                            [ Tiledmap.view map
                            , viewPlayer model.player
                            ]
                    )
                |> Maybe.withDefault (Html.text "NO MAP")
            ]
        ]


main : Program Value Model Msg
main =
    Browser.document
        { init = init
        , view =
            \model ->
                { title = "GGJ2020"
                , body = [ view model ]
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

        -- , if model.isPause then
        --     Sub.none
        --   else
        --     Browser.Events.onAnimationFrameDelta
        --         (\f ->
        --             if model.isPause then
        --                 PauseGame
        --             else
        --                 Tick <| f / 1000
        --         )
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
