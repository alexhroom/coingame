module Main (main) where

import Game
import Players
import Data.Ix
import System.Exit (exitWith)

main :: IO ()
main = do
    putStrLn "What size game?"
    gamesize <- getLine
    putStrLn "1 or 2 players?"
    numplayers <- getLine
    case numplayers of
        "1" -> gameLoop1Player (startGame (read gamesize)) "Player"
        "2" -> gameLoop2Player (startGame (read gamesize)) "Player 1" "Player 2"
        _ -> putStrLn "Please select 1 or 2"


-- main game loop for two-player game
gameLoop2Player :: [Bool] -> String -> String -> IO ()
gameLoop2Player game player opponent
    | all (== False) game =  putStrLn "Thanks for playing :)"
    | otherwise = do
        putStrLn (display game)
        putStrLn "Take a coin:"
        coin <- getLine
        if inRange (1, length game) (read coin) && game !! (read coin - 1)
            then do
                checkWin (turn game (read coin - 1)) player
                gameLoop2Player (turn game (read coin - 1)) opponent player
            else do
                putStrLn "Coin not valid."
                gameLoop2Player game player opponent

-- main game loop for CPU game
gameLoop1Player :: [Bool] -> String -> IO()
gameLoop1Player game player 
    | all (== False) game =  putStrLn "Thanks for playing :)"
    | otherwise = do
        putStrLn (display game)
        putStrLn "Take a coin:"
        coin <- getLine
        if inRange (1, length game) (read coin) && game !! (read coin - 1)
            then do
                let afterturn = (turn game (read coin - 1))
                checkWin afterturn player
                putStrLn ("CPU chooses coin " ++ show (halveStrategy afterturn))
                checkWin (turn afterturn (halveStrategy afterturn)) "CPU"
                gameLoop1Player (turn afterturn (halveStrategy afterturn)) player
            else do
                putStrLn "Coin not valid."
                gameLoop1Player game player

checkWin :: [Bool] -> String -> IO ()
checkWin game player
    | all (== False) game = do 
        putStrLn ("All coins gone! " ++ player ++ " wins!")
        exitWith 0
    | otherwise = return()
