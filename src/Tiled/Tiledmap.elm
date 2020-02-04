module Tiled.Tiledmap exposing (Tiledmap, decoder, mapHeight, mapWidth, tileHeight, tileWidth, view)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Tiled.Tile as Tile exposing (Tile)


type Tiledmap
    = Tiledmap Internal


type alias Internal =
    { height : Int
    , width : Int
    , tileHeight : Int
    , tileWidth : Int
    , layers : List Layer
    , tiles : Dict Int String
    }


type alias Layer =
    { name : String
    , tileIds : List Int
    }


tileHeight : Tiledmap -> Int
tileHeight (Tiledmap map) =
    map.tileHeight


tileWidth : Tiledmap -> Int
tileWidth (Tiledmap map) =
    map.tileWidth


mapHeight : Tiledmap -> Int
mapHeight (Tiledmap map) =
    map.height * map.tileHeight


mapWidth : Tiledmap -> Int
mapWidth (Tiledmap map) =
    map.width * map.tileWidth



--Render


view : Tiledmap -> Svg msg
view ((Tiledmap map) as tiledmap) =
    Svg.svg
        [ List.map String.fromInt [ 0, 0, mapWidth tiledmap, mapHeight tiledmap ]
            |> String.join " "
            |> Attributes.viewBox
        , Attributes.width "100%"
        ]
        [ Svg.svg [] <|
            List.map
                (\layer ->
                    Svg.g []
                        (List.indexedMap
                            (\tileIndex tileId ->
                                Tile.view tileIndex
                                    { tileName = Dict.get tileId map.tiles
                                    , height = map.tileHeight
                                    , width = map.tileWidth
                                    , mapCols = map.width
                                    }
                            )
                            layer.tileIds
                        )
                )
                map.layers
        ]



--DECODERS


decoder : Decoder Tiledmap
decoder =
    Decode.succeed Internal
        |> Decode.required "height" Decode.int
        |> Decode.required "width" Decode.int
        |> Decode.required "tileheight" Decode.int
        |> Decode.required "tilewidth" Decode.int
        |> Decode.required "layers" (Decode.list layerDecoder)
        |> Decode.custom tileToDictDecoder
        |> Decode.map Tiledmap


layerDecoder : Decoder Layer
layerDecoder =
    Decode.succeed Layer
        |> Decode.required "name" Decode.string
        |> Decode.required "data" (Decode.list Decode.int)


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
