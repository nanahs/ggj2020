module Tilemap exposing (Tilemap, drawMap, initTilemap, mapHeight, mapWidth)

import Svg
import Svg.Attributes as SvgAttrs
import Tile exposing (Tile)


type Tilemap
    = Tilemap TInternal


type alias TInternal =
    { tiles : List Tile
    , tileHeight : Int
    , tileWidth : Int
    , cols : Int
    , rows : Int
    }


initTilemap : { tiles : List Tile, tileHeight : Int, tileWidth : Int, cols : Int, rows : Int } -> Tilemap
initTilemap config =
    Tilemap config


mapHeight : Tilemap -> Int
mapHeight (Tilemap map) =
    map.tileHeight * map.rows


mapWidth : Tilemap -> Int
mapWidth (Tilemap map) =
    map.tileHeight * map.cols


drawMap : Tilemap -> Svg.Svg msg
drawMap (Tilemap tilemap) =
    Svg.svg
        [ SvgAttrs.width <| String.fromInt (tilemap.tileWidth * tilemap.cols)
        , SvgAttrs.height <| String.fromInt (tilemap.tileHeight * tilemap.rows)
        ]
    <|
        List.indexedMap
            (Tile.drawTile { height = tilemap.tileHeight, width = tilemap.tileWidth, mapCols = tilemap.cols })
            tilemap.tiles
