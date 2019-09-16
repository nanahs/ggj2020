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
            { h = 40, w = 10 }
    in
    { score = { p1 = 0, p2 = 0 }
    , player1 = { pos = Vector2.create { x = 0, y = (Vector2.getY gameBoard / 2) - h / 2 }, height = h, width = w }
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
    , radius = 5
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
        { x = 160
        , y = 192
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
                        { model | ball = initBall -1 }

                    else if model.ball.vel == Vector2.zero then
                        { model | ball = initBall 1 }

                    else
                        model
            , Cmd.none
            )

        --TODO use delta time to remove reliance on animatin frame rate
        Tick _ ->
            let
                p1OffsetY =
                    (*) 5 <|
                        case ( model.p1Down, model.p1Up ) of
                            ( Down, Up ) ->
                                1

                            ( Up, Down ) ->
                                -1

                            ( _, _ ) ->
                                0

                p2OffsetY =
                    (*) 5 <|
                        case ( model.p2Down, model.p2Up ) of
                            ( Down, Up ) ->
                                1

                            ( Up, Down ) ->
                                -1

                            ( _, _ ) ->
                                0

                paddleNewLoc pos offSet height =
                    Vector2.setY (Vector2.getY pos + offSet) pos
                        |> clampY 0 (Vector2.getY gameBoard - height)

                --Ball movement
                ballNewLoc =
                    Vector2.add model.ball.pos model.ball.vel
                        |> clampY model.ball.radius (Vector2.getY gameBoard - model.ball.radius)
                        |> clampX model.ball.radius (Vector2.getX gameBoard - model.ball.radius)

                maybeNewScore =
                    if Vector2.getX ballNewLoc <= model.ball.radius then
                        Just { p1 = model.score.p1, p2 = model.score.p2 + 1 }

                    else if Vector2.getX ballNewLoc >= Vector2.getX gameBoard - model.ball.radius then
                        Just { p1 = model.score.p1 + 1, p2 = model.score.p2 }

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
                , player1 =
                    { pos = paddleNewLoc model.player1.pos p1OffsetY model.player1.height
                    , height = model.player1.height
                    , width = model.player1.width
                    }
                , player2 =
                    { pos = paddleNewLoc model.player2.pos p2OffsetY model.player2.height
                    , height = model.player2.height
                    , width = model.player2.width
                    }
                , ball =
                    case maybeNewScore of
                        Just _ ->
                            initBall 0

                        Nothing ->
                            { pos = ballNewLoc
                            , vel =
                                if isRectCircCollision model.player1 model.ball && Vector2.getX model.ball.vel < 0 then
                                    let
                                        centerP1 =
                                            Vector2.setY
                                                (Vector2.getY model.player1.pos + model.player1.height / 2)
                                                model.player1.pos
                                    in
                                    Vector2.create { x = Vector2.getX model.ball.vel * -1, y = Vector2.getY <| Vector2.sub model.ball.pos centerP1 }

                                else if isRectCircCollision model.player2 model.ball && Vector2.getX model.ball.vel > 0 then
                                    let
                                        centerP2 =
                                            Vector2.setY
                                                (Vector2.getY model.player2.pos + model.player2.height / 2)
                                                model.player2.pos
                                    in
                                    Vector2.create { x = Vector2.getX model.ball.vel * -1, y = Vector2.getY <| Vector2.sub model.ball.pos centerP2 }

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


grayAttr : Svg.Attribute msg
grayAttr =
    SvgAttrs.class "text-gray-200 fill-current stroke-current"


blackAttr : Svg.Attribute msg
blackAttr =
    SvgAttrs.class "text-gray-900 fill-current"


viewBounds : Bool -> Svg.Svg msg
viewBounds isPaused =
    Svg.rect
        [ SvgAttrs.width <| String.fromFloat <| Vector2.getX gameBoard
        , SvgAttrs.height <| String.fromFloat <| Vector2.getY gameBoard
        , SvgAttrs.strokeWidth "1"
        , SvgAttrs.stroke "white"
        , if isPaused then
            SvgAttrs.class "text-gray-500 fill-current"

          else
            blackAttr
        ]
        []


viewMidLine : Svg.Svg msg
viewMidLine =
    Svg.line
        [ SvgAttrs.x1 <| String.fromFloat <| Vector2.getX gameBoard / 2
        , SvgAttrs.x2 <| String.fromFloat <| Vector2.getX gameBoard / 2
        , SvgAttrs.y1 "0"
        , SvgAttrs.y2 <| String.fromFloat <| Vector2.getY gameBoard
        , SvgAttrs.strokeWidth "1"
        , SvgAttrs.strokeDasharray "4 2"
        , grayAttr
        ]
        []


viewScore : Int -> Float -> Svg.Svg msg
viewScore score xPos =
    Svg.text_
        [ SvgAttrs.x <| String.fromFloat xPos
        , SvgAttrs.y "20"
        , SvgAttrs.class "text-gray-200 fill-current "
        ]
        [ Svg.text <| String.fromInt score ]


viewPaddle : Paddle -> Svg.Svg msg
viewPaddle paddle =
    Svg.rect
        [ SvgAttrs.height <| String.fromFloat paddle.height
        , SvgAttrs.width <| String.fromFloat paddle.width
        , SvgAttrs.x <| String.fromFloat <| Vector2.getX paddle.pos
        , SvgAttrs.y <| String.fromFloat <| Vector2.getY paddle.pos
        , grayAttr
        ]
        []


viewBall : Ball -> Svg.Svg msg
viewBall ball =
    Svg.circle
        [ SvgAttrs.r <| String.fromFloat ball.radius
        , SvgAttrs.cx <| String.fromFloat <| Vector2.getX ball.pos
        , SvgAttrs.cy <| String.fromFloat <| Vector2.getY ball.pos
        , grayAttr
        ]
        []


drawGame : Model -> Html Msg
drawGame model =
    Svg.svg
        [ List.map String.fromFloat [ 0, 0, Vector2.getX gameBoard, Vector2.getY gameBoard ]
            |> String.join " "
            |> SvgAttrs.viewBox
        , SvgAttrs.width "100%"
        ]
        [ viewBounds model.isPause
        , viewMidLine
        , viewScore model.score.p1 <| Vector2.getX gameBoard / 4
        , viewScore model.score.p2 <| (Vector2.getX gameBoard / 4) * 3
        , viewPaddle model.player1
        , viewPaddle model.player2
        , viewBall model.ball
        ]


view : Model -> Html Msg
view model =
    Html.div [ Attributes.class "flex justify-center h-screen bg-gray-900" ]
        [ Html.div [ Attributes.class "flex flex-col justify-center h-screen", Attributes.style "width" "600px" ]
            [ drawGame model ]
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
