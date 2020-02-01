module Player exposing (Player, initPlayer)

import Entity exposing (Entity)
import Vector2 exposing (Vector2)


type alias Player =
    { entity : Entity
    , canJump : Bool
    }


initPlayer : Vector2 -> Player
initPlayer pos =
    { entity = Entity.init pos Vector2.zero 10 10
    , canJump = True
    }
