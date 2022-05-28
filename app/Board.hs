module Board where

data Cell = E | X | O deriving (Eq)


data Board = Board Cell Cell Cell Cell Cell Cell Cell Cell Cell

rawBoardStr :: [Char]
rawBoardStr =
    "+---+---+---+\n\
    \| 1 | 2 | 3 |\n\
    \+---+---+---+\n\
    \| 4 | 5 | 6 |\n\
    \+---+---+---+\n\
    \| 7 | 8 | 9 |\n\
    \+---+---+---+"

instance Show Board where
    show (Board c1 c2 c3 c4 c5 c6 c7 c8 c9) = map replace rawBoardStr
      where
        replace :: Char -> Char
        replace c = case c of
            '1' -> cellToChar c1 '1'
            '2' -> cellToChar c2 '2'
            '3' -> cellToChar c3 '3'
            '4' -> cellToChar c4 '4'
            '5' -> cellToChar c5 '5'
            '6' -> cellToChar c6 '6'
            '7' -> cellToChar c7 '7'
            '8' -> cellToChar c8 '8'
            '9' -> cellToChar c9 '9'
            c   -> c
        cellToChar :: Cell -> Char -> Char
        cellToChar cell char = case cell of
            E -> char
            X -> 'X'
            O -> 'O'

boardToList :: Board -> [Cell]
boardToList (Board c1 c2 c3 c4 c5 c6 c7 c8 c9) =
    [c1, c2, c3, c4, c5, c6, c7, c8, c9]

boardToRows :: Board -> [[Cell]]
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

boardIsFull :: Board -> Bool
boardIsFull board = E `notElem` boardToList board

-- update the cell in board only if it is empty
updateBoard :: Board -> Cell -> Int -> Board
updateBoard board@(Board c1 c2 c3 c4 c5 c6 c7 c8 c9) cell num
    | num == 1 && c1 == E = Board cell c2 c3 c4 c5 c6 c7 c8 c9
    | num == 2 && c2 == E = Board c1 cell c3 c4 c5 c6 c7 c8 c9
    | num == 3 && c3 == E = Board c1 c2 cell c4 c5 c6 c7 c8 c9
    | num == 4 && c4 == E = Board c1 c2 c3 cell c5 c6 c7 c8 c9
    | num == 5 && c5 == E = Board c1 c2 c3 c4 cell c6 c7 c8 c9
    | num == 6 && c6 == E = Board c1 c2 c3 c4 c5 cell c7 c8 c9
    | num == 7 && c7 == E = Board c1 c2 c3 c4 c5 c6 cell c8 c9
    | num == 8 && c8 == E = Board c1 c2 c3 c4 c5 c6 c7 cell c9
    | num == 9 && c9 == E = Board c1 c2 c3 c4 c5 c6 c7 c8 cell
    | otherwise           = board


data Player = PlayerO | PlayerX

instance Show Player where
    show PlayerO = "Player O"
    show PlayerX = "Player x"


playerOWon :: Board -> Bool
playerOWon b = playerWon b PlayerO

playerXWon :: Board -> Bool
playerXWon b = playerWon b PlayerX

playerWon :: Board -> Player -> Bool
playerWon board player = True `elem` checkRows 
    where 
        c = cellOfPlayer player

        checkRows :: [Bool]
        checkRows = map (all (==c)) (boardToRows board)

cellOfPlayer :: Player -> Cell
cellOfPlayer player = case player of
    PlayerO -> O
    PlayerX -> X

nextPlayer :: Player -> Player
nextPlayer player = case player of
    PlayerO -> PlayerX
    PlayerX -> PlayerO
