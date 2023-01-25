module Game 
    ( startGame
    , turn
    , display
    ) where

import Control.Lens ( (.~), element )

-- Initialises a game.
startGame :: Int -> [Bool]
startGame n = replicate n True

-- takes a turn
turn :: [Bool] -> Int -> [Bool]
turn game x
    | x < 0 = game
    | x >= length game = game
    | game !! x = (element (x-1) .~ False) ((element x .~ False) ((element (x+1) .~ False) game))
    | otherwise = game

-- displays coins
display :: [Bool] -> [Char]
display = map boolAsCoin
    where boolAsCoin True  = '@'
          boolAsCoin False = '_'
