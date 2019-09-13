module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes as Attributes
import Json.Decode as Decode exposing (Decoder, Value)
import Svg as Svg
import Svg.Attributes as SvgAttrs
import Vector2 as Vector2 exposing (Vector2)


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
    | Input KeyState Input
    | Tick Float
    | PauseGame


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
    let
        { h, w } =
            { h = 75, w = 10 }
    in
    { score = ( 0, 0 )
    , player1 = { pos = Vector2.zero, height = h, width = w }
    , player2 = { pos = Vector2.sub gameBoard { x = w, y = (h // 2) + (gameBoard.y // 2) }, height = h, width = w }
    , ball = initBall
    , isPause = False
    , p1Up = Up
    , p1Down = Up
    }


initBall : Ball
initBall =
    { vel = { x = -5, y = 0 }
    , pos = { x = gameBoard.x // 2, y = gameBoard.y // 2 }
    , radius = 10
    }


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


isRectCircCollision : Paddle -> Ball -> Bool
isRectCircCollision paddle ball =
    let
        nearestX =
            max paddle.pos.x <| min ball.pos.x <| paddle.pos.x + paddle.width

        nearestY =
            max paddle.pos.y <| min ball.pos.y <| paddle.pos.y + paddle.height

        deltaX =
            ball.pos.x - nearestX

        deltaY =
            ball.pos.y - nearestY
    in
    (deltaX * deltaX + deltaY * deltaY) < (ball.radius * ball.radius)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateScore newScore ->
            ( { model | score = newScore }, Cmd.none )

        Input keyState input ->
            ( case input of
                MoveUp ->
                    { model | p1Up = keyState }

                MoveDown ->
                    { model | p1Down = keyState }

                Pause ->
                    { model | isPause = not model.isPause }
            , Cmd.none
            )

        --TODO use delta time to remove reliance on animatin frame rate
        Tick _ ->
            --updating positions only allowed in the Tick
            let
                loc =
                    model.player1.pos

                offsetY =
                    case ( model.p1Down, model.p1Up ) of
                        ( Down, Up ) ->
                            1

                        ( Up, Down ) ->
                            -1

                        ( _, _ ) ->
                            0

                newLoc =
                    clampY 0 (gameBoard.y - model.player1.height) { loc | y = loc.y + (offsetY * 5) }

                --Ball movement
                ballNewLoc =
                    Vector2.add model.ball.pos model.ball.vel
                        |> clampY model.ball.radius (gameBoard.y - model.ball.radius)
                        |> clampX model.ball.radius (gameBoard.x - model.ball.radius)
            in
            ( { model
                | player1 = { pos = newLoc, height = model.player1.height, width = model.player1.width }
                , ball =
                    { pos = ballNewLoc
                    , vel =
                        if isRectCircCollision model.player1 model.ball && model.ball.vel.x < 0 then
                            Vector2.scale model.ball.vel -1

                        else if isRectCircCollision model.player2 model.ball && model.ball.vel.x > 0 then
                            Vector2.scale model.ball.vel -1

                        else
                            model.ball.vel
                    , radius = model.ball.radius
                    }
              }
            , Cmd.none
            )

        PauseGame ->
            ( model, Cmd.none )


type KeyState
    = Up
    | Down


type Input
    = MoveUp
    | MoveDown
    | Pause


toInput : KeyState -> String -> Decoder Msg
toInput keyState str =
    case str of
        "ArrowUp" ->
            Decode.succeed <| Input keyState MoveUp

        "ArrowDown" ->
            Decode.succeed <| Input keyState MoveDown

        "Escape" ->
            case keyState of
                Up ->
                    Decode.succeed <| Input Up Pause

                _ ->
                    Decode.fail ""

        _ ->
            Decode.fail ""


init : Value -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )



-- VIEW


viewBounds : Bool -> Svg.Svg msg
viewBounds isPaused =
    Svg.rect
        [ SvgAttrs.width <| String.fromInt gameBoard.x
        , SvgAttrs.height <| String.fromInt gameBoard.y
        , SvgAttrs.strokeWidth "1"
        , SvgAttrs.stroke "black"
        , if isPaused then
            SvgAttrs.fill "grey"

          else
            SvgAttrs.fill "white"
        ]
        []


viewScore : ( Int, Int ) -> Svg.Svg msg
viewScore ( p1, p2 ) =
    Svg.text_
        [ SvgAttrs.x "440"
        , SvgAttrs.y "20"
        ]
        [ Svg.text <| String.join " " [ String.fromInt p1, "-", String.fromInt p2 ] ]


viewPaddle : Paddle -> Svg.Svg msg
viewPaddle paddle =
    -- make size and width screen relative
    Svg.rect
        [ SvgAttrs.height <| String.fromInt paddle.height
        , SvgAttrs.width <| String.fromInt paddle.width
        , SvgAttrs.x <| String.fromInt <| paddle.pos.x
        , SvgAttrs.y <| String.fromInt <| paddle.pos.y
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
            ]
            [ viewBounds model.isPause
            , viewScore model.score
            , viewPaddle model.player1
            , viewPaddle model.player2
            , viewBall model.ball
            ]
        ]


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
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown <|
            Decode.andThen (toInput Down) (Decode.field "key" Decode.string)
        , Browser.Events.onKeyUp <|
            Decode.andThen (toInput Up) (Decode.field "key" Decode.string)
        , Browser.Events.onAnimationFrameDelta
            (\f ->
                if model.isPause then
                    PauseGame

                else
                    Tick <| f / 1000
            )
        ]
