module Game exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Events
import Constants
import Dict exposing (Dict)
import Grid exposing (Grid)
import Html exposing (Html)
import Html.Attributes as Attributes
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import Layer exposing (Layer)
import Player exposing (Player)
import Svg as Svg
import Svg.Attributes as SvgAttrs
import Tiled.Tiledmap as Map exposing (Tiledmap)


type alias Model =
    { isPause : Bool
    , player : Player
    , background : Grid Int
    , collision : Grid Int
    , tiles : Dict Int String
    , height : Int
    , width : Int
    , tileWidth : Int
    , tileHeight : Int
    }


init : Tiledmap -> Model
init map =
    let
        width =
            Map.mapWidth map

        height =
            Map.mapHeight map
    in
    { isPause = False
    , player = Player.initPlayer <| ( 2, 2 )
    , background = initGrid height width (Map.background map)
    , collision = initGrid height width (Map.collisions map)
    , tiles = Map.tileset map
    , height = height
    , width = width
    , tileHeight = Map.tileHeight map
    , tileWidth = Map.tileWidth map

    -- , collisions = Grid.empty
    -- , map = map
    }


initGrid : Int -> Int -> Layer -> Grid Int
initGrid height width layer =
    layer
        |> .tileIds
        |> List.indexedMap
            (\index id ->
                ( ( modBy width index, index // width ), id )
            )
        |> Grid.init height width


type Msg
    = Input Input
    | Tick Float


type Input
    = MoveUp
    | MoveDown
    | MoveLeft
    | MoveRight



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input input ->
            ( updateInput model input, Cmd.none )

        Tick delta ->
            ( model, Cmd.none )


updateInput : Model -> Input -> Model
updateInput model input =
    if canMove model.collision model.player.pos input then
        { model
            | player =
                Player.setPos
                    (case input of
                        MoveUp ->
                            Grid.up model.player.pos

                        MoveDown ->
                            Grid.down model.player.pos

                        MoveLeft ->
                            Grid.left model.player.pos

                        MoveRight ->
                            Grid.right model.player.pos
                    )
                    model.player
        }

    else
        model


canMove : Grid Int -> ( Int, Int ) -> Input -> Bool
canMove collisions pos input =
    case input of
        MoveUp ->
            Grid.member (Grid.up pos) collisions

        MoveDown ->
            Grid.member (Grid.down pos) collisions

        MoveLeft ->
            Grid.member (Grid.left pos) collisions

        MoveRight ->
            Grid.member (Grid.right pos) collisions



-- VIEW


viewPlayer : Player -> Int -> Int -> Svg.Svg msg
viewPlayer player tileWidth tileHeight =
    Svg.image
        [ SvgAttrs.x <| String.fromInt <| (tileWidth * Tuple.first player.pos)
        , SvgAttrs.y <| String.fromInt <| (tileHeight * Tuple.second player.pos)
        , SvgAttrs.height <| String.fromInt tileHeight
        , SvgAttrs.width <| String.fromInt tileWidth
        , SvgAttrs.xlinkHref <| String.concat [ Constants.assetDir, player.src ]
        ]
        []


view : Model -> Html Msg
view model =
    Html.div [ Attributes.class "flex justify-center h-screen bg-gray-700" ]
        [ Html.div
            [ Attributes.class "flex flex-col justify-center h-screen w-full"
            ]
            [ Svg.svg
                [ List.map String.fromInt [ 0, 0, model.height * model.tileHeight, model.width * model.tileWidth ]
                    |> String.join " "
                    |> SvgAttrs.viewBox
                , SvgAttrs.width "100%"
                , SvgAttrs.height "100%"
                ]
              <|
                List.map (viewLayer model.tiles model.tileHeight model.tileWidth) [ model.background, model.collision ]
                    ++ [ viewPlayer model.player model.tileWidth model.tileHeight ]
            ]
        ]


viewLayer : Dict Int String -> Int -> Int -> Grid Int -> Svg.Svg msg
viewLayer tiles tileHeight tileWidth grid =
    Grid.toList grid
        |> List.map (viewTile tiles tileHeight tileWidth)
        |> Svg.g []


viewTile : Dict Int String -> Int -> Int -> ( ( Int, Int ), Int ) -> Svg.Svg msg
viewTile tiles tileHeight tileWidth ( ( x, y ), tile ) =
    Svg.image
        [ SvgAttrs.x <| String.fromInt (x * tileWidth)
        , SvgAttrs.y <| String.fromInt (y * tileHeight)
        , SvgAttrs.width <| String.fromInt tileWidth
        , SvgAttrs.height <| String.fromInt tileHeight
        , case Dict.get tile tiles of
            Just tileName ->
                SvgAttrs.xlinkHref <| String.concat [ Constants.assetDir, tileName ]

            Nothing ->
                Attributes.property "" Encode.null
        ]
        []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\str ->
                        case str of
                            "ArrowUp" ->
                                Decode.succeed (Input MoveUp)

                            "ArrowDown" ->
                                Decode.succeed (Input MoveDown)

                            "ArrowLeft" ->
                                Decode.succeed (Input MoveLeft)

                            "ArrowRight" ->
                                Decode.succeed (Input MoveRight)

                            "w" ->
                                Decode.succeed (Input MoveUp)

                            "s" ->
                                Decode.succeed (Input MoveDown)

                            "a" ->
                                Decode.succeed (Input MoveLeft)

                            "d" ->
                                Decode.succeed (Input MoveRight)

                            _ ->
                                Decode.fail ""
                    )
            )

        -- , Browser.Events.onAnimationFrameDelta (\f -> Tick <| f / 1000 )
        , Sub.none
        ]
