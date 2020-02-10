module Grid exposing (Grid, down, get, height, init, insert, left, member, remove, right, toList, up, update, width)

import Dict exposing (Dict)


type Grid a
    = Grid (Internal a)


type alias Internal a =
    { grid : Dict String a
    , height : Int
    , width : Int
    }


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



--


member : ( Int, Int ) -> Grid a -> Bool
member coord (Grid { grid }) =
    Dict.member (toString coord) grid


get : ( Int, Int ) -> Grid a -> Maybe a
get coord (Grid { grid }) =
    Dict.get (toString coord) grid


insert : ( Int, Int ) -> a -> Grid a -> Grid a
insert coord val (Grid grid) =
    Grid { grid | grid = Dict.insert (toString coord) val grid.grid }


update : ( Int, Int ) -> (Maybe a -> Maybe a) -> Grid a -> Grid a
update coord alter (Grid grid) =
    Grid { grid | grid = Dict.update (toString coord) alter grid.grid }


remove : ( Int, Int ) -> Grid a -> Grid a
remove coord (Grid grid) =
    Grid { grid | grid = Dict.remove (toString coord) grid.grid }


init : Int -> Int -> List ( ( Int, Int ), a ) -> Grid a
init h w list =
    Grid
        { grid =
            list
                |> List.map (Tuple.mapFirst toString)
                |> Dict.fromList
        , height = h
        , width = w
        }


toList : Grid a -> List ( ( Int, Int ), a )
toList (Grid { grid }) =
    Dict.toList grid
        |> List.filterMap
            (\( key, val ) ->
                Maybe.map (\k -> ( k, val )) (fromString key)
            )


height : Grid a -> Int
height (Grid grid) =
    grid.height


width : Grid a -> Int
width (Grid grid) =
    grid.width



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
