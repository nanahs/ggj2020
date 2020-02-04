module Vector2 exposing (Vector2, add, centerPoint, create, getX, getY, scale, setY, sub, zero)


type Vector2
    = Vector2
        { x : Int
        , y : Int
        }


create : { x : Int, y : Int } -> Vector2
create =
    Vector2


getX : Vector2 -> Int
getX (Vector2 { x }) =
    x


getY : Vector2 -> Int
getY (Vector2 { y }) =
    y


setY : Int -> Vector2 -> Vector2
setY y (Vector2 vec) =
    Vector2 { vec | y = y }


zero : Vector2
zero =
    Vector2 { x = 0, y = 0 }


add : Vector2 -> Vector2 -> Vector2
add (Vector2 f) (Vector2 s) =
    Vector2 { x = f.x + s.x, y = f.y + s.y }


sub : Vector2 -> Vector2 -> Vector2
sub (Vector2 f) (Vector2 s) =
    Vector2 { x = f.x - s.x, y = f.y - s.y }


scale : Int -> Vector2 -> Vector2
scale scaler (Vector2 { x, y }) =
    Vector2 { x = x * scaler, y = y * scaler }


centerPoint : { height : Int, width : Int } -> Vector2
centerPoint { height, width } =
    Vector2 { x = width // 2, y = height // 2 }
