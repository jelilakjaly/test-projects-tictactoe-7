module Main where


data Cell = E | X | O

data Board = Board Cell Cell Cell Cell Cell Cell Cell Cell Cell

main :: IO ()
main = putStrLn "Hello, Haskell!"
