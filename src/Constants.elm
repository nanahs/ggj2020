module Constants exposing (assetDir, mapList, tileSet)


assetDir : String
assetDir =
    "./public/assets/"


tileSet : String
tileSet =
    String.concat [ assetDir, "pokeTiles.tmx" ]


mapList : List String
mapList =
    [ "palletTown.json", "route1.json", "viridianCity.json" ]
