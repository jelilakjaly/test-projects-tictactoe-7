module Main where


import           Board


data GameStatus = Playing | OWon | XWon | NoWinner deriving Eq

instance Show GameStatus where
    show Playing  = "Playing"
    show OWon     = "Player O Won"
    show XWon     = "Player X Won"
    show NoWinner = "No one wins"

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

-- update the cell only if it is empty
updateBoard :: Board -> Player -> Int -> Board
updateBoard board@(Board c1 c2 c3 c4 c5 c6 c7 c8 c9) player num
    | num == 1 && c1 == E = Board (cellOfPlayer player) c2 c3 c4 c5 c6 c7 c8 c9
    | num == 2 && c2 == E = Board c1 (cellOfPlayer player) c3 c4 c5 c6 c7 c8 c9
    | num == 3 && c3 == E = Board c1 c2 (cellOfPlayer player) c4 c5 c6 c7 c8 c9
    | num == 4 && c4 == E = Board c1 c2 c3 (cellOfPlayer player) c5 c6 c7 c8 c9
    | num == 5 && c5 == E = Board c1 c2 c3 c4 (cellOfPlayer player) c6 c7 c8 c9
    | num == 6 && c6 == E = Board c1 c2 c3 c4 c5 (cellOfPlayer player) c7 c8 c9
    | num == 7 && c7 == E = Board c1 c2 c3 c4 c5 c6 (cellOfPlayer player) c8 c9
    | num == 8 && c8 == E = Board c1 c2 c3 c4 c5 c6 c7 (cellOfPlayer player) c9
    | num == 9 && c9 == E = Board c1 c2 c3 c4 c5 c6 c7 c8 (cellOfPlayer player)
    | otherwise           = board

{-
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
                        Just n  -> updateGame (updateBoard board player n)
                                              (nextPlayer player)
-}
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
                Just n  -> updateGame (updateBoard board player n)
                                      (nextPlayer player)


main :: IO ()
main = do
    let board  = Board E E E E E E E E E
    let player = PlayerO
    updateGame board player
