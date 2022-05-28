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

advanceGame :: Board -> Player -> IO ()
advanceGame board player = do
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
                Nothing -> advanceGame board player
                Just n ->
                    advanceGame (updateBoard board (cellOfPlayer player) n) (nextPlayer player)


main :: IO ()
main = do
    let board  = Board E E E E E E E E E
    let player = PlayerO
    advanceGame board player
