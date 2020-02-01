module Tile exposing (Tile, canOccupy, drawTile, empty, wallTop)

import Svg as Svg
import Svg.Attributes as SvgAttrs


type Tile
    = Tile Internal


type alias Internal =
    TileType


type TileType
    = WallTop
    | Empty


wallTop : Tile
wallTop =
    Tile WallTop


empty : Tile
empty =
    Tile Empty


canOccupy : Tile -> Bool
canOccupy (Tile tile) =
    case tile of
        WallTop ->
            False

        Empty ->
            True



--TILEMAP


drawTile : { height : Int, width : Int, mapCols : Int } -> Int -> Tile -> Svg.Svg msg
drawTile config index tile =
    Svg.svg
        [ SvgAttrs.width <| String.fromInt config.width
        , SvgAttrs.height <| String.fromInt config.height
        , SvgAttrs.x <| String.fromInt <| (*) config.width <| modBy config.mapCols index
        , SvgAttrs.y <| String.fromInt <| (*) config.height <| index // config.mapCols

        -- , SvgAttrs.class "fill-current text-red-500"
        ]
        [ drawT tile config.height config.width
        ]


drawT : Tile -> Int -> Int -> Svg.Svg msg
drawT (Tile tile) height width =
    case tile of
        WallTop ->
            Svg.g []
                [ Svg.title [] [ Svg.text "tile" ]
                , Svg.rect
                    [ SvgAttrs.width "100%"
                    , SvgAttrs.height "2"
                    , SvgAttrs.x "0"
                    , SvgAttrs.y "0"
                    , SvgAttrs.class "fill-current text-yellow-700"
                    ]
                    []
                ]

        Empty ->
            Svg.g [] []



-- viewPlayer : Player -> Svg.Svg msg
-- viewPlayer player =
--     Svg.rect
--         [ SvgAttrs.width <| String.fromFloat <| Entity.width player.entity
--         , SvgAttrs.height <| String.fromFloat <| Entity.height player.entity
--         , SvgAttrs.x <| String.fromFloat <| Vector2.getX <| Entity.position player.entity
--         , SvgAttrs.y <| String.fromFloat <| Vector2.getY <| Entity.position player.entity
--         , grayAttr
--         ]
--         []
