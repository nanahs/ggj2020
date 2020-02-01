module Main exposing (main)

import Browser
import Browser.Events
import Entity exposing (Entity)
import Html exposing (Html)
import Html.Attributes as Attributes
import Json.Decode as Decode exposing (Decoder, Value)
import Player exposing (Player)
import Svg as Svg
import Svg.Attributes as SvgAttrs
import Tile exposing (Tile)
import Tilemap exposing (Tilemap)
import Vector2 as Vector2 exposing (Vector2)


type alias Model =
    { isPause : Bool
    , player : Player
    , tilemap : Tilemap
    , jump : KeyState
    , left : KeyState
    , right : KeyState
    }


type Msg
    = Input KeyState Input
    | Tick Float
    | PauseGame


type KeyState
    = Up
    | Down


type Input
    = Jump
    | MoveLeft
    | MoveRight
    | Pause


init : Value -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { isPause = False
    , player = Player.initPlayer <| Vector2.create { x = 0, y = 16 * 6 }
    , tilemap =
        Tilemap.initTilemap
            { tiles =
                [ Tile.empty
                , Tile.empty
                , Tile.empty
                , Tile.empty
                , Tile.empty
                , Tile.empty
                , Tile.empty
                , Tile.empty
                , Tile.empty
                , Tile.empty
                , Tile.empty
                , Tile.empty
                , Tile.empty
                , Tile.empty
                , Tile.empty
                , Tile.empty
                , Tile.empty
                , Tile.empty
                , Tile.empty
                , Tile.empty
                , Tile.empty
                , Tile.empty
                , Tile.empty
                , Tile.empty
                , Tile.empty
                , Tile.empty
                , Tile.empty
                , Tile.empty
                , Tile.empty
                , Tile.empty
                , Tile.wallTop
                , Tile.wallTop
                , Tile.wallTop
                , Tile.wallTop
                , Tile.wallTop
                , Tile.wallTop
                ]
            , tileHeight = 16
            , tileWidth = 16
            , cols = 6
            , rows = 6
            }
    , jump = Up
    , left = Up
    , right = Up
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


gravity : Vector2
gravity =
    Vector2.create
        { x = 0
        , y = 1
        }


jumpForce : Float -> Vector2
jumpForce x =
    Vector2.create
        { x = x
        , y = -90
        }


moveSpeed : Vector2
moveSpeed =
    Vector2.create
        { x = 5
        , y = 0
        }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input keyState input ->
            ( case input of
                Jump ->
                    { model | jump = keyState }

                MoveLeft ->
                    { model | left = keyState }

                MoveRight ->
                    { model | right = keyState }

                Pause ->
                    { model | isPause = not model.isPause }
            , Cmd.none
            )

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


doJump : KeyState -> Player -> Player
doJump jump player =
    case ( jump, player.canJump ) of
        ( Down, True ) ->
            { player | entity = Entity.setVel (jumpForce <| Vector2.getX <| Entity.velocity player.entity) player.entity }

        _ ->
            { player | entity = Entity.updateVel gravity player.entity }


doMove : KeyState -> KeyState -> Player -> Player
doMove left right player =
    case ( left, right ) of
        ( Down, Up ) ->
            { player | entity = Entity.updateVel (Vector2.scale -1 moveSpeed) player.entity }

        ( Up, Down ) ->
            { player | entity = Entity.updateVel moveSpeed player.entity }

        ( Down, Down ) ->
            player

        ( Up, Up ) ->
            player



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


viewBounds : Tilemap -> Svg.Svg msg
viewBounds map =
    Svg.rect
        [ SvgAttrs.width <| String.fromInt <| Tilemap.mapWidth map
        , SvgAttrs.height <| String.fromInt <| Tilemap.mapHeight map
        , SvgAttrs.strokeWidth "1"
        , SvgAttrs.stroke "white"
        , blackAttr
        ]
        []


viewPlayer : Player -> Svg.Svg msg
viewPlayer player =
    Svg.rect
        [ SvgAttrs.width <| String.fromFloat <| Entity.width player.entity
        , SvgAttrs.height <| String.fromFloat <| Entity.height player.entity
        , SvgAttrs.x <| String.fromFloat <| Vector2.getX <| Entity.position player.entity
        , SvgAttrs.y <| String.fromFloat <| Vector2.getY <| Entity.position player.entity
        , grayAttr
        ]
        []


drawGame : Model -> Html Msg
drawGame model =
    Svg.svg
        [ List.map String.fromInt [ 0, 0, Tilemap.mapWidth model.tilemap, Tilemap.mapHeight model.tilemap ]
            |> String.join " "
            |> SvgAttrs.viewBox
        , SvgAttrs.width "100%"
        ]
        [ viewBounds model.tilemap
        , Tilemap.drawMap model.tilemap

        -- , viewPlayer model.player
        ]


view : Model -> Html Msg
view model =
    Html.div [ Attributes.class "flex justify-center h-screen bg-gray-700" ]
        [ Html.div
            [ Attributes.class "flex flex-col justify-center h-screen w-full"
            ]
            [ drawGame model ]
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
        , if model.isPause then
            Sub.none

          else
            Browser.Events.onAnimationFrameDelta
                (\f ->
                    if model.isPause then
                        PauseGame

                    else
                        Tick <| f / 1000
                )
        ]



-- Decoder


jumpDecoder : KeyState -> Decoder Msg
jumpDecoder keyState =
    Decode.succeed <| Input keyState Jump


leftDecoder : KeyState -> Decoder Msg
leftDecoder keyState =
    Decode.succeed <| Input keyState MoveLeft


rightDecoder : KeyState -> Decoder Msg
rightDecoder keyState =
    Decode.succeed <| Input keyState MoveRight


decodeInput : KeyState -> String -> Decoder Msg
decodeInput keyState str =
    case str of
        " " ->
            jumpDecoder keyState

        "ArrowUp" ->
            jumpDecoder keyState

        "ArrowLeft" ->
            leftDecoder keyState

        "ArrowRight" ->
            rightDecoder keyState

        "w" ->
            jumpDecoder keyState

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
