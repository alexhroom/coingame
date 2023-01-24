module Game 
    ( startGame
    , turn
    , display
    ) where

import Control.Lens

-- Initialises a game.
startGame :: Int -> [Bool]
startGame n = replicate n True

-- takes a turn
turn :: [Bool] -> Int -> [Bool]
turn game x
    | x < 0 = game
    | x >= (length game) = game
    | game !! x == True = (element (x-1) .~ False) ((element x .~ False) ((element (x+1) .~ False) game))
    | otherwise = game

-- displays coins
display :: [Bool] -> [Char]
display game = map boolAsCoin game
    where boolAsCoin True  = '@'
          boolAsCoin False = '_'
