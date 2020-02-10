module Player exposing (Player, initPlayer, setPos)


type alias Player =
    { pos : ( Int, Int )
    , src : String
    }


initPlayer : ( Int, Int ) -> Player
initPlayer pos =
    { pos = pos
    , src = "player.png"
    }


setPos : ( Int, Int ) -> Player -> Player
setPos pos player =
    { player | pos = pos }
