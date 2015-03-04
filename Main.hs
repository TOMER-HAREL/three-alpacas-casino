import qualified Test.HUnit as T
import Data.Char
import qualified Games.BlackJack as BJ
import qualified Games.Poker as P5
import System.Console.ANSI

import Card
import Hand
import Game
import Deck
import Player
import Interface

main :: IO ()
main = do
  home

home :: IO ()
home = do
  clearScreen
  printLnCenter "Welcome to Playboy Casino."
  printGameTable everyGame
  putStr " [q to quit]: "
  rawLine <- getLine
  let userAction = filterUserInput (map toUpper rawLine)
  if userAction == "Q" then do --quit casino
    putStrLn "Exiting Playboy Casino.."
  else if userAction == "" then do --if the string is empty, handle crash
    gameDoesntExist
  else do -- if it's not a user action but a game choice
    let gameNumber = read userAction :: Int
    if validGameEnum gameNumber then do
      let game = (toEnum gameNumber :: Game)
      putStrLn (show game)
      if game == BJ then
        BJ.main
      else if game == P5 then
        P5.main
      else
        return ()
      -- TODO load game.main

    else --if the number isn't mapped to a game
      gameDoesntExist

gameDoesntExist :: IO ()
gameDoesntExist = do
  putStrLn "Game doesn't exist"
  home

printGameTable :: [Game] -> IO ()
printGameTable [] = putStrLn "Playboy Casino is out of poison."
printGameTable games =
  let
    printGameTable' :: [Game] -> Int -> IO ()
    printGameTable' [] _ = return ()
    printGameTable' (game:rest) acc = do
      putStrLn ("[" ++ show(acc) ++ "] " ++ show(game))
      printGameTable' rest (acc + 1)
  in
    do
      printDivider
      printGameTable' games 1
      putStr("Please pick your poison [1 - " ++ show(gameCount) ++ "]")

{-
  TODO: add actions that are valid
-}
userActions :: String
userActions = ['Q'] -- TODO map to a datatype instead

{-
  PURPOSE: return a string with every valid number that are mapped to a game.
-}
userGames :: String
userGames = map (head . show) [1 .. gameCount]

{-
  PURPOSE: combine actions and valid games into one string.
-}
validUserInput :: String
validUserInput = userActions ++ userGames

{-
  PURPOSE: remove elements that doesn't match valid user input.
-}
filterUserInput :: String -> String
filterUserInput input = filter (\character -> elem character validUserInput) input

{- TESTS -}
runtests = T.runTestTT $ T.TestList [testListHand, testListDeck, testListCard, BJ.testListBJ, P5.testListP5]
