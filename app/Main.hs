module Main where


data Cell = E | X | O deriving (Eq)

instance Show Cell where 
    show E = ""
    show X = "X"
    show O = "O"

charOf :: Cell -> Char 
charOf E = ' '
charOf X = 'X'
charOf O = 'O'

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
                '1' -> charOf c1
                '2' -> charOf c2
                '3' -> charOf c3
                '4' -> charOf c4
                '5' -> charOf c5
                '6' -> charOf c6 
                '7' -> charOf c7
                '8' -> charOf c8
                '9' -> charOf c9
                c   -> c


data Player = PlayerO | PlayerX deriving (Eq)

getWinner :: Board -> Maybe Player
getWinner = undefined

playerOWon :: Board -> Bool
playerOWon b = playerWon b PlayerO

playerXWon :: Board -> Bool
playerXWon b = playerWon b PlayerX

playerWon :: Board -> Player -> Bool 
playerWon (Board c1 c2 c3 c4 c5 c6 c7 c8 c9) player = 
    (c1 == c && c2 == c && c3 == c) ||
    (c4 == c && c5 == c && c6 == c) ||
    (c7 == c && c8 == c && c9 == c) ||
    (c1 == c && c4 == c && c7 == c) ||
    (c2 == c && c5 == c && c8 == c) ||
    (c3 == c && c6 == c && c9 == c) ||
    (c1 == c && c5 == c && c9 == c) ||
    (c3 == c && c5 == c && c7 == c)
    where
        c = case player of
          PlayerO -> O
          PlayerX -> X



main :: IO ()
main = putStrLn "Hello, Haskell!"
