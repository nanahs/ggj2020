module Layer exposing (Layer, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode


type alias Layer =
    { name : String
    , tileIds : List Int
    }



-- DECODER


decoder : String -> Decoder Layer
decoder layerName =
    Decode.field "layers" (Decode.list decodeHelp)
        |> Decode.andThen
            (\layerList ->
                List.foldl
                    (\val acc ->
                        if val.name == layerName then
                            Decode.succeed val

                        else
                            acc
                    )
                    (Decode.fail "")
                    layerList
            )


decodeHelp : Decoder Layer
decodeHelp =
    Decode.succeed Layer
        |> Decode.required "name" Decode.string
        |> Decode.required "data" (Decode.list Decode.int)
