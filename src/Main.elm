module Main exposing (main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attributes
import Json.Decode as Decode exposing (Decoder, Value)
import Svg as Svg
import Svg.Attributes as SvgAttrs


type alias Model =
    { score : ( Int, Int )
    , player1 : Vector2
    , player2 : Vector2
    , ball : PhysObj
    , isPause : Bool
    , p1Up : KeyState
    , p1Down : KeyState
    }


type Msg
    = UpdateScore ( Int, Int )
    | TogglePause
    | Input KeyState Input
    | Tick Float
    | NoOp


type alias Vector2 =
    { x : Int
    , y : Int
    }


type alias PhysObj =
    { pos : Vector2
    , vel : Vector2
    }


initialModel : Model
initialModel =
    { score = ( 0, 0 )
    , player1 = vector2Center paddleSize
    , player2 = vector2Sub gameBoard (vector2Center paddleSize)
    , ball = initBall
    , isPause = False
    , p1Up = Up
    , p1Down = Up
    }


initBall : PhysObj
initBall =
    { vel = { x = -5, y = 0 }
    , pos = { x = gameBoard.x // 2, y = gameBoard.y // 2 }
    }


vector2Zero : Vector2
vector2Zero =
    { x = 0, y = 0 }


vector2Add : Vector2 -> Vector2 -> Vector2
vector2Add f s =
    { x = f.x + s.x, y = f.y + s.y }


vector2Sub : Vector2 -> Vector2 -> Vector2
vector2Sub f s =
    { x = f.x - s.x, y = f.y - s.y }


vector2Center : Vector2 -> Vector2
vector2Center { x, y } =
    --mid point assuming the x and y are width and height
    { x = x // 2, y = y // 2 }


clampY : Int -> Int -> Vector2 -> Vector2
clampY min max vect2 =
    if vect2.y < min then
        { x = vect2.x, y = min }

    else if vect2.y > max then
        { x = vect2.x, y = max }

    else
        vect2


clampX : Int -> Int -> Vector2 -> Vector2
clampX min max vect2 =
    if vect2.x < min then
        { x = min, y = vect2.y }

    else if vect2.x > max then
        { x = max, y = vect2.y }

    else
        vect2


gameBoard : Vector2
gameBoard =
    { x = 896
    , y = 504
    }


paddleSize : Vector2
paddleSize =
    { x = 10
    , y = 75
    }


ballRadius : Int
ballRadius =
    10



-- VIEW


paddle : Vector2 -> Svg.Svg msg
paddle pos =
    -- make size and width screen relative
    let
        posOffset =
            vector2Center paddleSize
    in
    Svg.rect
        [ SvgAttrs.height <| String.fromInt paddleSize.y
        , SvgAttrs.width <| String.fromInt paddleSize.x
        , SvgAttrs.cy "37.5"
        , SvgAttrs.cx "5"
        , SvgAttrs.x <| String.fromInt <| pos.x - posOffset.x
        , SvgAttrs.y <| String.fromInt <| pos.y - posOffset.y
        ]
        []


ball : Vector2 -> Svg.Svg msg
ball pos =
    let
        posOffset =
            vector2Center { x = ballRadius, y = ballRadius }
    in
    Svg.circle
        [ SvgAttrs.r <| String.fromInt ballRadius
        , SvgAttrs.fill "black"
        , SvgAttrs.cx <| String.fromInt <| pos.x - posOffset.x
        , SvgAttrs.cy <| String.fromInt <| pos.y - posOffset.y
        ]
        []


view : Model -> Html Msg
view model =
    Html.div [ Attributes.class "m-4" ]
        [ Svg.svg
            [ SvgAttrs.width <| String.fromInt gameBoard.x
            , SvgAttrs.height <| String.fromInt gameBoard.y
            , SvgAttrs.class "border border-black"
            ]
            [ paddle model.player1
            , paddle model.player2
            , ball model.ball.pos
            ]
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateScore newScore ->
            ( { model | score = newScore }, Cmd.none )

        TogglePause ->
            ( { model | isPause = not model.isPause }, Cmd.none )

        Input keyState input ->
            ( case input of
                MoveUp ->
                    { model | p1Up = keyState }

                MoveDown ->
                    { model | p1Down = keyState }
            , Cmd.none
            )

        --TODO use delta time to remove reliance on animatin frame rate
        Tick _ ->
            --updating positions only allowed in the Tick
            let
                loc =
                    model.player1

                paddleCenter =
                    vector2Center paddleSize

                offsetY =
                    case ( model.p1Down, model.p1Up ) of
                        ( Down, Up ) ->
                            1

                        ( Up, Down ) ->
                            -1

                        ( _, _ ) ->
                            0

                newLoc =
                    clampY paddleCenter.y (gameBoard.y - paddleCenter.y) { loc | y = loc.y + (offsetY * 5) }

                --Ball movement
                ballNewLoc =
                    vector2Add model.ball.pos model.ball.vel
                        |> clampY ballRadius (gameBoard.y - ballRadius)
                        |> clampX ballRadius (gameBoard.x - ballRadius)
            in
            ( { model
                | player1 = newLoc
                , ball = { pos = ballNewLoc, vel = model.ball.vel }
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


type KeyState
    = Up
    | Down


type Input
    = MoveUp
    | MoveDown


toInput : KeyState -> String -> Decoder Msg
toInput keyState str =
    case str of
        "ArrowUp" ->
            Decode.succeed <| Input keyState MoveUp

        "ArrowDown" ->
            Decode.succeed <| Input keyState MoveDown

        _ ->
            Decode.fail ""


inputToId : Input -> String
inputToId input =
    case input of
        MoveUp ->
            "MoveUp"

        MoveDown ->
            "MoveDown"


init : Value -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


main : Program Value Model Msg
main =
    Browser.document
        { init = init
        , view =
            \model ->
                { title = "PONG"
                , body = [ view model ]
                }
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown <|
            Decode.andThen (toInput Down) (Decode.field "key" Decode.string)
        , Browser.Events.onKeyUp <|
            Decode.andThen (toInput Up) (Decode.field "key" Decode.string)
        , Browser.Events.onAnimationFrameDelta (\f -> Tick <| f / 1000)
        ]
