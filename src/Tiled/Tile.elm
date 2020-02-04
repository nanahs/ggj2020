module Tiled.Tile exposing (Tile, decoder, view)

import Constants
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Svg as Svg exposing (Svg)
import Svg.Attributes as Attributes


type alias Tile =
    { id : Int
    , imgName : String
    }


view : Int -> { tileName : Maybe String, height : Int, width : Int, mapCols : Int } -> Svg msg
view index { tileName, height, width, mapCols } =
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



--Decoder


decoder : Decoder Tile
decoder =
    Decode.succeed Tile
        |> Decode.required "id" Decode.int
        |> Decode.required "image" Decode.string
