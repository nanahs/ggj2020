module Tiled.Tiledmap exposing (Tiledmap, background, collisions, decoder, mapHeight, mapWidth, tileHeight, tileWidth, tileset, view)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Layer exposing (Layer)
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Tiled.Tile as Tile exposing (Tile)


type Tiledmap
    = Tiledmap Internal


type alias Internal =
    { info : MapInfo
    , background : Layer
    , collisions : Layer
    , tiles : Dict Int String
    }


type alias MapInfo =
    { height : Int
    , width : Int
    , tileHeight : Int
    , tileWidth : Int
    }


tileHeight : Tiledmap -> Int
tileHeight (Tiledmap map) =
    map.info.tileHeight


tileWidth : Tiledmap -> Int
tileWidth (Tiledmap map) =
    map.info.tileWidth


mapHeight : Tiledmap -> Int
mapHeight (Tiledmap map) =
    map.info.height


mapWidth : Tiledmap -> Int
mapWidth (Tiledmap map) =
    map.info.width


tileset : Tiledmap -> Dict Int String
tileset (Tiledmap map) =
    map.tiles


background : Tiledmap -> Layer
background (Tiledmap map) =
    map.background


collisions : Tiledmap -> Layer
collisions (Tiledmap map) =
    map.collisions



--Render


view : Tiledmap -> Svg msg
view ((Tiledmap map) as tiledmap) =
    Svg.svg
        [ List.map String.fromInt [ 0, 0, mapWidth tiledmap, mapHeight tiledmap ]
            |> String.join " "
            |> Attributes.viewBox
        , Attributes.width "100%"
        ]
        [ drawLayer map.background tiledmap
        , drawLayer map.collisions tiledmap
        ]


drawLayer : Layer -> Tiledmap -> Svg msg
drawLayer layer (Tiledmap map) =
    Svg.g [ Attributes.title layer.name ] <|
        List.indexedMap
            (\index id ->
                Tile.view index
                    { tileName = Dict.get id map.tiles
                    , height = map.info.tileHeight
                    , width = map.info.tileWidth
                    , mapCols = map.info.width
                    }
            )
            layer.tileIds



--DECODERS


decoder : Decoder Tiledmap
decoder =
    Decode.succeed Internal
        |> Decode.custom decodeMapInfo
        |> Decode.custom (Layer.decoder "Background")
        |> Decode.custom (Layer.decoder "Collision")
        |> Decode.custom tileToDictDecoder
        |> Decode.map Tiledmap


decodeMapInfo : Decoder MapInfo
decodeMapInfo =
    Decode.succeed MapInfo
        |> Decode.required "height" Decode.int
        |> Decode.required "width" Decode.int
        |> Decode.required "tileheight" Decode.int
        |> Decode.required "tilewidth" Decode.int


tileToDictDecoder : Decoder (Dict Int String)
tileToDictDecoder =
    Decode.field "tilesets" (Decode.list tilesetDecoder)
        |> Decode.map
            (\listOfList ->
                listOfList
                    |> List.concat
                    --the tile id needs to be incremented by 1 because 0 is a special case of no tile
                    |> List.map (\tile -> ( tile.id + 1, tile.imgName ))
                    |> Dict.fromList
            )


tilesetDecoder : Decoder (List Tile)
tilesetDecoder =
    Decode.field "tiles" (Decode.list Tile.decoder)
