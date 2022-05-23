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

instance Show Player where 
    show PlayerO = "Player O"
    show PlayerX = "Player x"

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

boardIsFull :: Board -> Bool 
boardIsFull (Board c1 c2 c3 c4 c5 c6 c7 c8 c9) =
    c1 /= E && c2 /= E && c3 /= E &&
    c4 /= E && c5 /= E && c6 /= E &&
    c7 /= E && c8 /= E && c9 /= E

-- clears the terminal on linux
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"


strToNumber :: String -> Maybe Int 
strToNumber n = case n of 
    "1" -> Just 1
    "2" -> Just 2
    "3" -> Just 3
    "4" -> Just 4
    "5" -> Just 5
    "6" -> Just 6
    "7" -> Just 7
    "8" -> Just 8
    "9" -> Just 9
    _   -> Nothing

updateBoard :: Board -> Player -> Int -> Board 
updateBoard board@(Board c1 c2 c3 c4 c5 c6 c7 c8 c9) player num = 
    case num of
        1 -> if c1 == E
                then Board (cellOfPlayer player) c2 c3 c4 c5 c6 c7 c8 c9
                else board
        2 -> if c2 == E
                then Board c1 (cellOfPlayer player) c3 c4 c5 c6 c7 c8 c9
                else board
        3 -> if c3 == E
                then Board c1 c2 (cellOfPlayer player) c4 c5 c6 c7 c8 c9
                else board
        4 -> if c4 == E
                then Board c1 c2 c3 (cellOfPlayer player) c5 c6 c7 c8 c9
                else board
        5 -> if c5 == E
                then Board c1 c2 c3 c4 (cellOfPlayer player) c6 c7 c8 c9
                else board
        6 -> if c6 == E
                then Board c1 c2 c3 c4 c5 (cellOfPlayer player) c7 c8 c9
                else board
        7 -> if c7 == E
                then Board c1 c2 c3 c4 c5 c6 (cellOfPlayer player) c8 c9
                else board
        8 -> if c8 == E
                then Board c1 c2 c3 c4 c5 c6 c7 (cellOfPlayer player) c9
                else board
        9 -> if c9 == E
                then Board c1 c2 c3 c4 c5 c6 c7 c8 (cellOfPlayer player)
                else board
        _ -> board

    where 
        cellOfPlayer :: Player -> Cell
        cellOfPlayer player = case player of 
            PlayerO -> O
            playerX -> X

nextPlayer :: Player -> Player 
nextPlayer player = case player of
  PlayerO -> PlayerX
  PlayerX -> PlayerO

updateGame :: Board -> Player -> IO ()
updateGame board player = do 
    clearScreen
    putStrLn (show board)
    if playerOWon board 
        then do 
            putStrLn "Player O wins."
            return ()
        else if playerXWon board 
                 then do 
                     putStrLn "Player X wins."
                     return ()
                 else if boardIsFull board 
                          then do
                              putStrLn "Nobody wins!"
                              return ()
                           else do 
                              putStrLn (show player ++ ", mark your position (1-9)")
                              numStr <- getLine 
                              case strToNumber numStr of
                                  Nothing -> updateGame board player
                                  Just n -> updateGame (updateBoard board player n) (nextPlayer player)



main :: IO ()
main = do 
    let board = Board E E E E E E E E E
    let player = PlayerO
    updateGame board player
