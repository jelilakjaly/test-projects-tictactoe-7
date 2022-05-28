module Main where

import           Board


data GameStatus = Playing | OWon | XWon | NoWinner deriving Eq

getGameStatus :: Board -> GameStatus
getGameStatus board | playerOWon board  = OWon
                    | playerXWon board  = XWon
                    | boardIsFull board = NoWinner
                    | otherwise         = Playing

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


updateGame :: Board -> Player -> IO ()
updateGame board player = do
    clearScreen
    putStrLn $ show board
    case getGameStatus board of
        OWon -> do
            putStrLn "Player O Wins."
            return ()
        XWon -> do
            putStrLn "Player X Wins."
            return ()
        NoWinner -> do
            putStrLn "No one wins."
            return ()
        Playing -> do
            putStrLn (show player ++ ", mark your position (1-9)")
            numStr <- getLine
            case strToNumber numStr of
                Nothing -> updateGame board player
                Just n ->
                    updateGame (updateBoard board (cellOfPlayer player) n) (nextPlayer player)


main :: IO ()
main = do
    let board  = Board E E E E E E E E E
    let player = PlayerO
    updateGame board player
