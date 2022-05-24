module Board where

data Cell = E | X | O deriving (Eq)

instance Show Cell where
    show E = ""
    show X = "X"
    show O = "O"

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


data Player = PlayerO | PlayerX

instance Show Player where
    show PlayerO = "Player O"
    show PlayerX = "Player x"


playerOWon :: Board -> Bool
playerOWon b = playerWon b PlayerO

playerXWon :: Board -> Bool
playerXWon b = playerWon b PlayerX

playerWon :: Board -> Player -> Bool
playerWon (Board c1 c2 c3 c4 c5 c6 c7 c8 c9) player =
    (c1 == c && c2 == c && c3 == c)
        || (c4 == c && c5 == c && c6 == c)
        || (c7 == c && c8 == c && c9 == c)
        || (c1 == c && c4 == c && c7 == c)
        || (c2 == c && c5 == c && c8 == c)
        || (c3 == c && c6 == c && c9 == c)
        || (c1 == c && c5 == c && c9 == c)
        || (c3 == c && c5 == c && c7 == c)
  where
    c = case player of
        PlayerO -> O
        PlayerX -> X



nextPlayer :: Player -> Player
nextPlayer player = case player of
    PlayerO -> PlayerX
    PlayerX -> PlayerO
