module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes as Attributes
import Json.Decode as Decode exposing (Decoder, Value)
import Svg as Svg
import Svg.Attributes as SvgAttrs


type alias Model =
    { score : ( Int, Int )
    , player1 : Paddle
    , player2 : Paddle
    , ball : Ball
    , isPause : Bool
    , p1Up : KeyState
    , p1Down : KeyState
    }


type Msg
    = UpdateScore ( Int, Int )
    | TogglePause
    | Input KeyState Input
    | Tick Float


type alias Vector2 =
    { x : Int
    , y : Int
    }


type alias Ball =
    { pos : Vector2
    , vel : Vector2
    , radius : Int
    }


type alias Paddle =
    { height : Int
    , width : Int
    , pos : Vector2
    }


initialModel : Model
initialModel =
    { score = ( 0, 0 )
    , player1 = { pos = centerPoint { height = 75, width = 10 }, height = 75, width = 10 }
    , player2 = { pos = vector2Sub gameBoard (centerPoint { height = 75, width = 10 }), height = 75, width = 10 }
    , ball = initBall
    , isPause = False
    , p1Up = Up
    , p1Down = Up
    }


initBall : Ball
initBall =
    { vel = { x = -1, y = 0 }
    , pos = { x = gameBoard.x // 2, y = gameBoard.y // 2 }
    , radius = 10
    }


vector2Add : Vector2 -> Vector2 -> Vector2
vector2Add f s =
    { x = f.x + s.x, y = f.y + s.y }


vector2Sub : Vector2 -> Vector2 -> Vector2
vector2Sub f s =
    { x = f.x - s.x, y = f.y - s.y }


vector2Scale : Vector2 -> Int -> Vector2
vector2Scale { x, y } scale =
    { x = x * scale, y = y * scale }


centerPoint : { height : Int, width : Int } -> Vector2
centerPoint { height, width } =
    --mid point assuming the x and y are width and height
    { x = width // 2, y = height // 2 }


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



-- VIEW


viewPaddle : Paddle -> Svg.Svg msg
viewPaddle paddle =
    -- make size and width screen relative
    let
        posOffset =
            centerPoint { height = paddle.height, width = paddle.width }
    in
    Svg.rect
        [ SvgAttrs.height <| String.fromInt paddle.height
        , SvgAttrs.width <| String.fromInt paddle.width
        , SvgAttrs.x <| String.fromInt <| paddle.pos.x - posOffset.x
        , SvgAttrs.y <| String.fromInt <| paddle.pos.y - posOffset.y
        ]
        []


viewBall : Ball -> Svg.Svg msg
viewBall ball =
    Svg.circle
        [ SvgAttrs.r <| String.fromInt ball.radius
        , SvgAttrs.fill "black"
        , SvgAttrs.cx <| String.fromInt <| ball.pos.x
        , SvgAttrs.cy <| String.fromInt <| ball.pos.y
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
            [ viewPaddle model.player1
            , viewPaddle model.player2
            , viewBall model.ball
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
                    model.player1.pos

                paddleCenter =
                    centerPoint { height = model.player1.height, width = model.player1.width }

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
                        |> clampY model.ball.radius (gameBoard.y - model.ball.radius)
                        |> clampX model.ball.radius (gameBoard.x - model.ball.radius)

                isPaddleCollision p1 p2 b bVel =
                    if bVel.x < 0 then
                        if b.x - p1.x < model.ball.radius + paddleCenter.x then
                            True

                        else
                            False

                    else if p2.x - b.x < model.ball.radius + paddleCenter.x then
                        True

                    else
                        False
            in
            ( { model
                | player1 = { pos = newLoc, height = model.player1.height, width = model.player1.width }
                , ball =
                    { pos = ballNewLoc
                    , vel =
                        if isPaddleCollision newLoc model.player2.pos ballNewLoc model.ball.vel then
                            vector2Scale model.ball.vel -1

                        else
                            model.ball.vel
                    , radius = model.ball.radius
                    }
              }
            , Cmd.none
            )


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
