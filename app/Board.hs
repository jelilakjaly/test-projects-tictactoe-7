module Board where
import           Data.List                      ( insert )

data Cell = E Int | X Int | O Int deriving (Eq)

isE :: Cell -> Bool
isE (E _) = True
isE _     = False

isO :: Cell -> Bool
isO (O _) = True
isO _     = False

isX :: Cell -> Bool
isX (X _) = True
isX _     = False

indexOf :: Cell -> Int
indexOf (E i) = i
indexOf (O i) = i
indexOf (X i) = i

data Board a = Board a a a a a a a a a

boardLine = "+---------+---------+---------+"

makeLines :: Cell -> Cell -> Cell -> [Char]
makeLines c1 c2 c3 =
    makeLine1 c1 c2 c3
        ++ "\n"
        ++ makeLine2 c1 c2 c3
        ++ "\n"
        ++ makeLine3 c1 c2 c3

makeLine1 :: Cell -> Cell -> Cell -> [Char]
makeLine1 c1 c2 c3 =
    "|" ++ makePiece1 c1 ++ "|" ++ makePiece1 c2 ++ "|" ++ makePiece1 c3 ++ "|"

makeLine2 :: Cell -> Cell -> Cell -> [Char]
makeLine2 c4 c5 c6 =
    "|" ++ makePiece2 c4 ++ "|" ++ makePiece2 c5 ++ "|" ++ makePiece2 c6 ++ "|"

makeLine3 :: Cell -> Cell -> Cell -> [Char]
makeLine3 c7 c8 c9 =
    "|" ++ makePiece3 c7 ++ "|" ++ makePiece3 c8 ++ "|" ++ makePiece3 c9 ++ "|"

makePiece1 :: Cell -> [Char]
makePiece1 (E _) = "         "
makePiece1 (O _) = "  * * *  "
makePiece1 (X _) = "    *    "

makePiece2 :: Cell -> [Char]
makePiece2 (E i) = "    " ++ show i ++ "    "
makePiece2 (O _) = "  * * *  "
makePiece2 (X _) = "  * * *  "

makePiece3 :: Cell -> [Char]
makePiece3 (E _) = "         "
makePiece3 (O _) = "  * * *  "
makePiece3 (X _) = "    *    "


drawBoard :: Board Cell -> String
drawBoard (Board c1 c2 c3 c4 c5 c6 c7 c8 c9) =
    boardLine
        ++ "\n"
        ++ makeLines c1 c2 c3
        ++ "\n"
        ++ boardLine
        ++ "\n"
        ++ makeLines c4 c5 c6
        ++ "\n"
        ++ boardLine
        ++ "\n"
        ++ makeLines c7 c8 c9
        ++ "\n"
        ++ boardLine
        ++ "\n"


boardToList :: Board Cell -> [Cell]
boardToList (Board c1 c2 c3 c4 c5 c6 c7 c8 c9) =
    [c1, c2, c3, c4, c5, c6, c7, c8, c9]

boardToRows :: Board Cell -> [[Cell]]
boardToRows (Board c1 c2 c3 c4 c5 c6 c7 c8 c9) =
    [ [c1, c2, c3]
    , [c4, c5, c6]
    , [c7, c8, c9]
    , [c1, c4, c7]
    , [c2, c5, c8]
    , [c3, c6, c9]
    , [c1, c5, c9]
    , [c3, c5, c7]
    ]

boardMap :: (Cell -> Cell) -> Board Cell -> Board Cell
boardMap f (Board c1 c2 c3 c4 c5 c6 c7 c8 c9) =
    Board (f c1) (f c2) (f c3) (f c4) (f c5) (f c6) (f c7) (f c8) (f c9)

boardIsFull :: Board Cell -> Bool
boardIsFull board = not (any isE (boardToList board))

updateBoard :: Board Cell -> Cell -> Board Cell
updateBoard board cell = boardMap func board
  where
    func :: Cell -> Cell
    func c@(E i) = if indexOf cell == i then cell else c
    func x       = x


data Player = PlayerO | PlayerX

instance Show Player where
    show PlayerO = "Player O"
    show PlayerX = "Player x"


playerOWon :: Board Cell -> Bool
playerOWon b = playerWon b PlayerO

playerXWon :: Board Cell -> Bool
playerXWon b = playerWon b PlayerX

playerWon :: Board Cell -> Player -> Bool
playerWon board player = True `elem` checkRows
  where
    checkRows :: [Bool]
    checkRows = map (all isFunc) (boardToRows board)

    isFunc :: (Cell -> Bool)
    isFunc = case player of
        PlayerO -> isO
        PlayerX -> isX

cellOfPlayer :: Player -> Int -> Cell
cellOfPlayer player index = case player of
    PlayerO -> O index
    PlayerX -> X index

nextPlayer :: Player -> Player
nextPlayer player = case player of
    PlayerO -> PlayerX
    PlayerX -> PlayerO
