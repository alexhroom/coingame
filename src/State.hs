module State 
    ( GameState
    , gameToGameState
    ) where

{-Defining GameState type and operation-}
data GameState = Coins Int | Gap Int deriving (Show, Eq)

instance Ord GameState where
    compare (Coins x) (Coins y) = compare x y
    compare (Gap x) (Gap y) = compare x y
    compare (Coins x) (Gap y) = compare x y
    compare (Gap x) (Coins y) = compare x y

sameType :: GameState -> GameState -> Bool
sameType (Coins _) (Coins _) = True
sameType (Gap _) (Gap _) = True
sameType _ _ = False

-- operator to reduce two GameStates to one
(.^) :: GameState -> GameState -> GameState 
(.^) (Coins x) (Coins y) = (Coins (x+y))
(.^) (Gap x) (Gap y) = (Gap (x+y))
(.^) _ _ = error "Can't add coins to gap in GameState"


{- Creating functions to turn a game into a GameState -}
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