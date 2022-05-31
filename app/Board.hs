module Board where

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

data Row a = Row a a a
    deriving Eq

instance Functor Row where
    fmap f (Row x1 x2 x3) = Row (f x1) (f x2) (f x3)

instance Foldable Row where
    foldr f b (Row x1 x2 x3) = f x3 $ f x2 $ f x1 b


data Board a = Board a a a a a a a a a

boardLine :: [Char]
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

instance Functor Board where
    fmap f (Board x1 x2 x3 x4 x5 x6 x7 x8 x9) =
        Board (f x1) (f x2) (f x3) (f x4) (f x5) (f x6) (f x7) (f x8) (f x9)

instance Foldable Board where
    foldr f b (Board a1 a2 a3 a4 a5 a6 a7 a8 a9) =
        f a9 $ f a8 $ f a7 $ f a6 $ f a5 $ f a4 $ f a3 $ f a2 $ f a1 b


boardToRows :: Board Cell -> [Row Cell]
boardToRows (Board c1 c2 c3 c4 c5 c6 c7 c8 c9) =
    [ Row c1 c2 c3
    , Row c4 c5 c6
    , Row c7 c8 c9
    , Row c1 c4 c7
    , Row c2 c5 c8
    , Row c3 c6 c9
    , Row c1 c5 c9
    , Row c3 c5 c7
    ]

boardIsFull :: Board Cell -> Bool
boardIsFull board = not $ any isE board

updateBoard :: Board Cell -> Cell -> Board Cell
updateBoard board cell = fmap func board
  where
    func :: Cell -> Cell
    func c@(E i) = if indexOf cell == i then cell else c
    func x       = x


data Player = PlayerO | PlayerX

instance Show Player where
    show PlayerO = "Player O"
    show PlayerX = "Player x"


playerOWon :: Board Cell -> Bool
playerOWon board = won $ boardToRows board
   where 
       won [] = False 
       won (row:rest) = all isO row || won rest

playerXWon :: Board Cell -> Bool
playerXWon board = won $ boardToRows board
    where 
        won [] = False
        won (row:rest) = all isX row || won rest

nextPlayer :: Player -> Player
nextPlayer player = case player of
    PlayerO -> PlayerX
    PlayerX -> PlayerO
