module Player exposing (Player, initPlayer)

import Vector2 exposing (Vector2)


type alias Player =
    { pos : Vector2
    }


initPlayer : Vector2 -> Player
initPlayer pos =
    { pos = pos
    }
