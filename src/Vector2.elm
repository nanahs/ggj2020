module Vector2 exposing (Vector2, add, centerPoint, create, getX, getY, normalize, scale, setY, sub, zero)


type Vector2
    = Vector2
        { x : Float
        , y : Float
        }


create : { x : Float, y : Float } -> Vector2
create =
    Vector2


getX : Vector2 -> Float
getX (Vector2 { x, y }) =
    x


getY : Vector2 -> Float
getY (Vector2 { x, y }) =
    y


setY : Float -> Vector2 -> Vector2
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


normalize : Vector2 -> Vector2
normalize (Vector2 { x, y }) =
    let
        len =
            sqrt <| (x * x) + (y * y)
    in
    Vector2 { x = x / len, y = y / len }


scale : Float -> Vector2 -> Vector2
scale scaler (Vector2 { x, y }) =
    Vector2 { x = x * scaler, y = y * scaler }


centerPoint : { height : Float, width : Float } -> Vector2
centerPoint { height, width } =
    Vector2 { x = width / 2, y = height / 2 }
