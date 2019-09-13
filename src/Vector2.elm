module Vector2 exposing (Vector2, add, centerPoint, scale, sub, zero)


type alias Vector2 =
    { x : Int
    , y : Int
    }


zero : Vector2
zero =
    { x = 0, y = 0 }


add : Vector2 -> Vector2 -> Vector2
add f s =
    { x = f.x + s.x, y = f.y + s.y }


sub : Vector2 -> Vector2 -> Vector2
sub f s =
    { x = f.x - s.x, y = f.y - s.y }


scale : Vector2 -> Int -> Vector2
scale { x, y } scaler =
    { x = x * scaler, y = y * scaler }


centerPoint : { height : Int, width : Int } -> Vector2
centerPoint { height, width } =
    { x = width // 2, y = height // 2 }
