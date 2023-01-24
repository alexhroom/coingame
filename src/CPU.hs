module CPU where

import Dict exposing (Dict)
import Game

CPUs : Dict String ([Bool] -> [Bool])
CPUs = Dict.fromList 
    [("1", danny)
    ]

-- "Danny Divide", CPU player who always
-- takes from the middle of the longest
-- remaining row of coins
danny :: [Bool] -> [Bool]
danny turn = do
    