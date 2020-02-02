module Tiledmap exposing (Tiledmap, decoder, mapHeight, mapWidth, view)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Svg exposing (Svg)
import Svg.Attributes as Attributes


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


type alias Tile =
    { id : Int
    , imgName : String
    }


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
            List.indexedMap
                (\layerIndex layer ->
                    Svg.g []
                        (List.indexedMap
                            (\tileIndex tileId ->
                                viewTile tileIndex
                                    { tileName = Dict.get tileId map.tiles
                                    , tileHeight = map.tileHeight
                                    , tileWidth = map.tileWidth
                                    , mapCols = map.width
                                    }
                            )
                            layer.tileIds
                        )
                )
                map.layers
        ]


viewTile : Int -> { tileName : Maybe String, tileHeight : Int, tileWidth : Int, mapCols : Int } -> Svg msg
viewTile index { tileName, tileHeight, tileWidth, mapCols } =
    Svg.svg
        [ Attributes.width <| String.fromInt tileWidth
        , Attributes.height <| String.fromInt tileHeight
        , Attributes.x <| String.fromInt <| (*) tileWidth <| modBy mapCols index
        , Attributes.y <| String.fromInt <| (*) tileHeight <| index // mapCols
        ]
        [ Svg.image
            [ Attributes.width <| String.fromInt tileWidth
            , Attributes.height <| String.fromInt tileHeight
            , case tileName of
                Just imageName ->
                    Attributes.xlinkHref <| String.concat [ "./public/assets/", imageName ]

                Nothing ->
                    Attributes.title "empty tile"
            ]
            []
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
        |> Decode.custom tDec
        |> Decode.map Tiledmap


layerDecoder : Decoder Layer
layerDecoder =
    Decode.succeed Layer
        |> Decode.required "name" Decode.string
        |> Decode.required "data" (Decode.list Decode.int)


tileDecoder : Decoder Tile
tileDecoder =
    Decode.succeed Tile
        |> Decode.required "id" Decode.int
        |> Decode.required "image" Decode.string


tDec : Decoder (Dict Int String)
tDec =
    Decode.field "tilesets" (Decode.list ts)
        |> Decode.map
            (\listOfList ->
                listOfList
                    |> List.concat
                    --the tile id needs to be incremented by 1 because 0 is a special case of no tile
                    |> List.map (\tile -> ( tile.id + 1, tile.imgName ))
                    |> Dict.fromList
            )


ts : Decoder (List Tile)
ts =
    Decode.field "tiles" (Decode.list tileDecoder)



--EXPERIMENTAL
-- type alias TileLayer =
--     { tiles : List Tile
--     , layerName : String
--     }
-- tileLayerDecoder : Decoder (List TileLayer)
-- tileLayerDecoder =
--     Decode.at [ "tilesets", "tiles" ] (Decode.list tileDecoder)
--         |> Decode.andThen tileLayerHelper
-- tileLayerHelper : List Tile -> Decoder (List TileLayer)
-- tileLayerHelper tiles =
--     Decode.field "layers" (Decode.list (newLayerDecoder tiles))
-- newLayerDecoder : List Tile -> Decoder TileLayer
-- newLayerDecoder tiles =
--     Decode.succeed (TileLayer tiles)
--         |> Decode.required "name" Decode.string
