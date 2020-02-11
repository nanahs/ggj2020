module Grid exposing
    ( Grid
    , down
    , get
    , init
    , insert
    , left
    , member
    , remove
    , right
    , section
    , toList
    , toString
    , up
    , update
    )

import Dict exposing (Dict)


type Grid a
    = Grid (Dict String a)


up : ( Int, Int ) -> ( Int, Int )
up ( x, y ) =
    ( x, y - 1 )


down : ( Int, Int ) -> ( Int, Int )
down ( x, y ) =
    ( x, y + 1 )


left : ( Int, Int ) -> ( Int, Int )
left ( x, y ) =
    ( x - 1, y )


right : ( Int, Int ) -> ( Int, Int )
right ( x, y ) =
    ( x + 1, y )


section : ( Int, Int ) -> Int -> Int -> Grid a -> Grid a
section ( x, y ) camWidth camHeight (Grid grid) =
    let
        ( camX, camY ) =
            ( (-) x <| (camWidth + 1) // 2, (-) y <| (camHeight + 1) // 2 )
    in
    List.foldl
        (\coord acc ->
            case Dict.get (toString coord) grid of
                Just tile ->
                    Dict.insert (toString coord) tile acc

                Nothing ->
                    acc
        )
        Dict.empty
        (sectionHelper camX camY camWidth camHeight)
        |> Grid


sectionHelper : Int -> Int -> Int -> Int -> List ( Int, Int )
sectionHelper camX camY camWidth camHeight =
    List.range camX (camX + camWidth)
        |> List.map
            (\x ->
                List.range camY (camY + camHeight)
                    |> List.map (\y -> ( x, y ))
            )
        |> List.concat


isEven : Int -> Bool
isEven =
    (==) 0 << modBy 2



--


member : ( Int, Int ) -> Grid a -> Bool
member coord (Grid grid) =
    Dict.member (toString coord) grid


get : ( Int, Int ) -> Grid a -> Maybe a
get coord (Grid grid) =
    Dict.get (toString coord) grid


insert : ( Int, Int ) -> a -> Grid a -> Grid a
insert coord val (Grid grid) =
    Grid <| Dict.insert (toString coord) val grid


update : ( Int, Int ) -> (Maybe a -> Maybe a) -> Grid a -> Grid a
update coord alter (Grid grid) =
    Grid <| Dict.update (toString coord) alter grid


remove : ( Int, Int ) -> Grid a -> Grid a
remove coord (Grid grid) =
    Grid <| Dict.remove (toString coord) grid


init : Int -> Int -> List ( ( Int, Int ), a ) -> Grid a
init h w list =
    list
        |> List.map (Tuple.mapFirst toString)
        |> Dict.fromList
        |> Grid


toList : Grid a -> List ( ( Int, Int ), a )
toList (Grid grid) =
    Dict.toList grid
        |> List.filterMap
            (\( key, val ) ->
                Maybe.map (\k -> ( k, val )) (fromString key)
            )



-- height : Grid a -> Int
-- height (Grid grid) =
--     grid.height
-- width : Grid a -> Int
-- width (Grid grid) =
--     grid.width
-- INTERNALS


toString : ( Int, Int ) -> String
toString ( x, y ) =
    String.join ","
        [ String.fromInt x
        , String.fromInt y
        ]


fromString : String -> Maybe ( Int, Int )
fromString key =
    case List.map String.toInt (String.split "," key) of
        [ Just x, Just y ] ->
            Just ( x, y )

        _ ->
            Nothing
