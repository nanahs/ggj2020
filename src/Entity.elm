module Entity exposing (Entity, height, init, position, setVel, updatePos, updateVel, velocity, width)

import Vector2 exposing (Vector2)


type Entity
    = Entity Internal


type alias Internal =
    { pos : Vector2
    , vel : Vector2
    , height : Float
    , width : Float
    , idle : Bool
    }


position : Entity -> Vector2
position (Entity entity) =
    entity.pos


velocity : Entity -> Vector2
velocity (Entity entity) =
    entity.vel


height : Entity -> Float
height (Entity entity) =
    entity.height


width : Entity -> Float
width (Entity entity) =
    entity.width


init : Vector2 -> Vector2 -> Float -> Float -> Entity
init pos vel h w =
    Entity
        { pos = pos
        , vel = vel
        , height = h
        , width = w
        , idle = True
        }


updatePos : Float -> Vector2 -> Vector2 -> Entity -> Entity
updatePos delta minClamp maxClamp (Entity entity) =
    let
        newPos =
            Vector2.add entity.pos (Vector2.scale delta entity.vel)
                |> clampPos minClamp maxClamp entity.width entity.height
    in
    Entity { entity | pos = newPos, idle = entity.pos == newPos }


updateVel : Vector2 -> Entity -> Entity
updateVel vel (Entity entity) =
    Entity { entity | vel = Vector2.add entity.vel vel }


setVel : Vector2 -> Entity -> Entity
setVel vel (Entity entity) =
    Entity { entity | vel = vel }


clampPos : Vector2 -> Vector2 -> Float -> Float -> Vector2 -> Vector2
clampPos min max h w pos =
    Vector2.create
        { x = Vector2.getX <| clampX (Vector2.getX min) (Vector2.getX max - w) pos
        , y = Vector2.getY <| clampY (Vector2.getY min) (Vector2.getY max - h) pos
        }


clampY : Float -> Float -> Vector2 -> Vector2
clampY min max vect2 =
    if Vector2.getY vect2 < min then
        Vector2.create { x = Vector2.getX vect2, y = min }

    else if Vector2.getY vect2 > max then
        Vector2.create { x = Vector2.getX vect2, y = max }

    else
        vect2


clampX : Float -> Float -> Vector2 -> Vector2
clampX min max vect2 =
    if Vector2.getX vect2 < min then
        Vector2.create { x = min, y = Vector2.getY vect2 }

    else if Vector2.getX vect2 > max then
        Vector2.create { x = max, y = Vector2.getY vect2 }

    else
        vect2
