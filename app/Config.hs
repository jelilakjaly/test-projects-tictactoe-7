module Config where

data BoardSize = Small | Big deriving (Show, Eq)

newtype Config = Config {
    boardSize :: BoardSize
}
