module Players 
    ( halveStrategy
    ) where

import State
import Game (turn)

{-
Halve is a CPU which simply takes the largest continuous section of coins,
and selects the middle coin.
-}
-- strategy of the Halve CPU
halveStrategy :: [Bool] -> Int
halveStrategy game = middle (getGamePartLoc (getLargestCoins gamestate) gamestate)
    where gamestate = gameToGameState game
          middle (_, y, _) = y
