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
    { score :
        { p1 : Int
        , p2 : Int
        }
    , player1 : Paddle
    , player2 : Paddle
    , ball : Ball
    , isPause : Bool
    , p1Up : KeyState
    , p1Down : KeyState
    , p2Up : KeyState
    , p2Down : KeyState
    , p1RecentScore : Bool
    }


type Msg
    = Input KeyState Input
    | Tick Float
    | PauseGame


type alias Ball =
    { pos : Vector2
    , vel : Vector2
    , radius : Float
    }


type alias Paddle =
    { height : Float
    , width : Float
    , pos : Vector2
    }


type KeyState
    = Up
    | Down


type Input
    = P1MoveUp
    | P1MoveDown
    | P2MoveUp
    | P2MoveDown
    | Pause
    | MoveBall


init : Value -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    let
        { h, w } =
            { h = 75, w = 10 }
    in
    { score = { p1 = 0, p2 = 0 }
    , player1 = { pos = Vector2.zero, height = h, width = w }
    , player2 = { pos = Vector2.sub gameBoard <| Vector2.create { x = w, y = (h / 2) + (Vector2.getY gameBoard / 2) }, height = h, width = w }
    , ball = initBall 0
    , isPause = False
    , p1Up = Up
    , p1Down = Up
    , p2Up = Up
    , p2Down = Up
    , p1RecentScore = True
    }


initBall : Float -> Ball
initBall xVel =
    { vel = Vector2.create { x = xVel, y = 0 }
    , pos = Vector2.create { x = Vector2.getX gameBoard / 2, y = Vector2.getY gameBoard / 2 }
    , radius = 10
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


gameBoard : Vector2
gameBoard =
    Vector2.create
        { x = 896
        , y = 504
        }


isRectCircCollision : Paddle -> Ball -> Bool
isRectCircCollision paddle ball =
    let
        nearestX =
            max (Vector2.getX paddle.pos) <|
                min (Vector2.getX ball.pos) <|
                    Vector2.getX paddle.pos
                        + paddle.width

        nearestY =
            max (Vector2.getY paddle.pos) <|
                min (Vector2.getY ball.pos) <|
                    Vector2.getY paddle.pos
                        + paddle.height

        deltaX =
            Vector2.getX ball.pos - nearestX

        deltaY =
            Vector2.getY ball.pos - nearestY
    in
    (deltaX * deltaX + deltaY * deltaY) < (ball.radius * ball.radius)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input keyState input ->
            ( case input of
                P1MoveUp ->
                    { model | p1Up = keyState }

                P1MoveDown ->
                    { model | p1Down = keyState }

                P2MoveUp ->
                    { model | p2Up = keyState }

                P2MoveDown ->
                    { model | p2Down = keyState }

                Pause ->
                    { model | isPause = not model.isPause }

                MoveBall ->
                    if model.p1RecentScore && model.ball.vel == Vector2.zero then
                        { model | ball = initBall -5 }

                    else if model.ball.vel == Vector2.zero then
                        { model | ball = initBall 5 }

                    else
                        model
            , Cmd.none
            )

        --TODO use delta time to remove reliance on animatin frame rate
        Tick _ ->
            let
                p1Loc =
                    model.player1.pos

                p2Loc =
                    model.player2.pos

                p1OffsetY =
                    case ( model.p1Down, model.p1Up ) of
                        ( Down, Up ) ->
                            1

                        ( Up, Down ) ->
                            -1

                        ( _, _ ) ->
                            0

                p2OffsetY =
                    case ( model.p2Down, model.p2Up ) of
                        ( Down, Up ) ->
                            1

                        ( Up, Down ) ->
                            -1

                        ( _, _ ) ->
                            0

                paddleNewLoc =
                    clampY 0 (Vector2.getY gameBoard - model.player1.height)

                p1NewLoc =
                    paddleNewLoc <| Vector2.setY (Vector2.getY p1Loc + (p1OffsetY * 5)) p1Loc

                p2NewLoc =
                    paddleNewLoc <| Vector2.setY (Vector2.getY p2Loc + (p2OffsetY * 5)) p2Loc

                --Ball movement
                ballNewLoc =
                    Vector2.add model.ball.pos model.ball.vel
                        |> clampY model.ball.radius (Vector2.getY gameBoard - model.ball.radius)
                        |> clampX model.ball.radius (Vector2.getX gameBoard - model.ball.radius)

                score =
                    model.score

                maybeNewScore =
                    if Vector2.getX ballNewLoc <= model.ball.radius then
                        Just { score | p2 = score.p2 + 1 }

                    else if Vector2.getX ballNewLoc >= Vector2.getX gameBoard - model.ball.radius then
                        Just { score | p1 = score.p1 + 1 }

                    else
                        Nothing
            in
            ( { model
                | score =
                    case maybeNewScore of
                        Just newScore ->
                            newScore

                        Nothing ->
                            model.score
                , player1 = { pos = p1NewLoc, height = model.player1.height, width = model.player1.width }
                , player2 = { pos = p2NewLoc, height = model.player2.height, width = model.player2.width }
                , ball =
                    case maybeNewScore of
                        Just _ ->
                            initBall 0

                        Nothing ->
                            { pos = ballNewLoc
                            , vel =
                                if isRectCircCollision model.player1 model.ball && Vector2.getX model.ball.vel < 0 then
                                    Vector2.sub model.ball.pos
                                        (Vector2.setY
                                            (Vector2.getY model.player1.pos + model.player1.height / 2)
                                            model.player1.pos
                                        )
                                        |> Vector2.normalize
                                        |> Vector2.scale 5

                                else if isRectCircCollision model.player2 model.ball && Vector2.getX model.ball.vel > 0 then
                                    Vector2.sub model.ball.pos
                                        (Vector2.setY
                                            (Vector2.getY model.player2.pos + model.player2.height / 2)
                                            model.player2.pos
                                        )
                                        |> Vector2.normalize
                                        |> Vector2.scale 5

                                else if Vector2.getY ballNewLoc <= model.ball.radius then
                                    Vector2.setY (Vector2.getY model.ball.vel * -1) model.ball.vel

                                else if Vector2.getY ballNewLoc >= Vector2.getY gameBoard - model.ball.radius then
                                    Vector2.setY (Vector2.getY model.ball.vel * -1) model.ball.vel

                                else
                                    model.ball.vel
                            , radius = model.ball.radius
                            }
                , p1RecentScore =
                    case maybeNewScore of
                        Just newScore ->
                            model.score.p1 /= newScore.p1

                        Nothing ->
                            model.p1RecentScore
              }
            , Cmd.none
            )

        PauseGame ->
            ( model, Cmd.none )



-- VIEW


viewBounds : Bool -> Svg.Svg msg
viewBounds isPaused =
    Svg.rect
        [ SvgAttrs.width <| String.fromFloat <| Vector2.getX gameBoard
        , SvgAttrs.height <| String.fromFloat <| Vector2.getY gameBoard
        , SvgAttrs.strokeWidth "1"
        , SvgAttrs.stroke "black"
        , if isPaused then
            SvgAttrs.fill "grey"

          else
            SvgAttrs.fill "white"
        ]
        []


viewScore : { p1 : Int, p2 : Int } -> Svg.Svg msg
viewScore { p1, p2 } =
    Svg.text_
        [ SvgAttrs.x "440"
        , SvgAttrs.y "20"
        ]
        [ Svg.text <| String.join " " [ String.fromInt p1, "-", String.fromInt p2 ] ]


viewPaddle : Paddle -> Svg.Svg msg
viewPaddle paddle =
    -- make size and width screen relative
    Svg.rect
        [ SvgAttrs.height <| String.fromFloat paddle.height
        , SvgAttrs.width <| String.fromFloat paddle.width
        , SvgAttrs.x <| String.fromFloat <| Vector2.getX paddle.pos
        , SvgAttrs.y <| String.fromFloat <| Vector2.getY paddle.pos
        ]
        []


viewBall : Ball -> Svg.Svg msg
viewBall ball =
    Svg.circle
        [ SvgAttrs.r <| String.fromFloat ball.radius
        , SvgAttrs.fill "black"
        , SvgAttrs.cx <| String.fromFloat <| Vector2.getX ball.pos
        , SvgAttrs.cy <| String.fromFloat <| Vector2.getY ball.pos
        ]
        []


view : Model -> Html Msg
view model =
    Html.div [ Attributes.class "m-4" ]
        [ Svg.svg
            [ SvgAttrs.width <| String.fromFloat <| Vector2.getX gameBoard
            , SvgAttrs.height <| String.fromFloat <| Vector2.getY gameBoard
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


decodeInput : KeyState -> String -> Decoder Msg
decodeInput keyState str =
    case str of
        "ArrowUp" ->
            Decode.succeed <| Input keyState P1MoveUp

        "ArrowDown" ->
            Decode.succeed <| Input keyState P1MoveDown

        "w" ->
            Decode.succeed <| Input keyState P2MoveUp

        "s" ->
            Decode.succeed <| Input keyState P2MoveDown

        "Escape" ->
            case keyState of
                Up ->
                    Decode.succeed <| Input Up Pause

                _ ->
                    Decode.fail ""

        " " ->
            case keyState of
                Up ->
                    Decode.succeed <| Input Up MoveBall

                _ ->
                    Decode.fail ""

        _ ->
            Decode.fail ""
