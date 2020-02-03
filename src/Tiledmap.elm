module Tiledmap exposing (Tiledmap, decoder, mapHeight, mapWidth, tileHeight, tileWidth, view)

import Constants
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
                                viewTile tileIndex
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



-- Can I use something like this to create more efficient html structure??


indexedMap2 : (Int -> a -> b -> result) -> List a -> List b -> List result
indexedMap2 f listA listB =
    let
        longestLength =
            if List.length listB > List.length listA then
                List.length listB - 1

            else
                List.length listA - 1
    in
    List.map3 f (List.range 0 longestLength) listA listB


viewTile : Int -> { tileName : Maybe String, height : Int, width : Int, mapCols : Int } -> Svg msg
viewTile index { tileName, height, width, mapCols } =
    Svg.svg
        [ Attributes.width <| String.fromInt height
        , Attributes.height <| String.fromInt width
        , Attributes.x <| String.fromInt <| (*) width <| modBy mapCols index
        , Attributes.y <| String.fromInt <| (*) height <| index // mapCols
        ]
        [ Svg.image
            [ Attributes.width <| String.fromInt width
            , Attributes.height <| String.fromInt height
            , case tileName of
                Just imageName ->
                    Attributes.xlinkHref <| String.concat [ Constants.assetDir, imageName ]

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
