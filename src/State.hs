module State 
    ( GameState
    , gameToGameState
    , getGamePartLoc
    , getLargestCoins
    ) where

{-Defining GameState type and operation-}
data GameState = Coins Int | Gap Int deriving (Show, Eq)

instance Ord GameState where
    compare :: GameState -> GameState -> Ordering
    compare x y = compare (size x) (size y)

sameType :: GameState -> GameState -> Bool
sameType (Coins _) (Coins _) = True
sameType (Gap _) (Gap _) = True
sameType _ _ = False

size :: GameState -> Int
size (Coins x) = x
size (Gap x) = x

-- operator to reduce two GameStates to one equivalent representation
(.^) :: GameState -> GameState -> GameState 
(.^) (Coins x) (Coins y) = Coins (x+y)
(.^) (Gap x) (Gap y) = Gap (x+y)
(.^) _ _ = error "Can't add coins to gap in GameState"


{- Functions to turn a game into a GameState -}
-- converts bool to GameState
boolToGameState :: Bool -> GameState
boolToGameState True = Coins 1
boolToGameState False = Gap 1

-- reduce gameState to values of Coins and Gaps
reduceGameState :: [GameState] -> [GameState]
reduceGameState [] = []
reduceGameState [x] = [x]
reduceGameState [x, y]
    | sameType x y = [x.^y]
    | otherwise = [x, y]
reduceGameState (x:y:zs)
    | sameType x y = reduceGameState ((x.^y):zs)
    | otherwise = x : reduceGameState (y:zs)

-- turns whole game to a gamestate list
gameToGameState :: [Bool] -> [GameState]
gameToGameState game = reduceGameState (map boolToGameState game)


{- functions for investigating a GameState array -}
-- get the start position of a specific GameState section
getGamePartMin :: GameState -> [GameState] -> Int
getGamePartMin part (x:xs)
    | part == x = 0
    | otherwise = size x + getGamePartMin part xs
getGamePartMin _ _ = 0

-- get start, middle, and end position of a GameState section
getGamePartLoc :: GameState -> [GameState] -> (Int, Int, Int)
getGamePartLoc part game = (start, start + div (size part) 2, start + size part)
    where start = getGamePartMin part game

-- gets largest continuous row of coins in the game
getLargestCoins :: [GameState] -> GameState
getLargestCoins [] = error "Can't get largest coins of empty list"
getLargestCoins game = Coins (maximum 
                              (map size 
                                (filter 
                                  (sameType (Coins 1)) game)))