module Player exposing (Player, initPlayer)


type alias Player =
    { pos : ( Int, Int )
    , src : String
    }


initPlayer : ( Int, Int ) -> Player
initPlayer pos =
    { pos = pos
    , src = "player.png"
    }
