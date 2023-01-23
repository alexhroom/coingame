module Main (main) where

import Game
import Data.Ix

main :: IO ()
main = do
    putStrLn "What size game?"
    gamesize <- getLine
    gameLoop (startGame (read gamesize))

-- main game loop
gameLoop :: [Bool] -> IO ()
gameLoop game 
    | all (== False) game =  putStrLn "All coins gone!"
    | otherwise = do
        putStrLn (display game)
        putStrLn "Take a coin:"
        coin <- getLine
        if inRange (1, length game) (read coin) && game !! (read coin)
            then gameLoop (turn game (read coin - 1))
            else do
                putStrLn "Coin not valid."
                gameLoop game

