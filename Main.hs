module Main where

  import qualified Test.HUnit as T
  import Data.Char
  import qualified Games.BlackJack as BJ
  import qualified Games.Poker as P5
  import System.Console.ANSI
  import Card
  import Hand
  import Game
  import Deck
  import qualified Player as P
  import Interface

  {-
    main
    PURPOSE: to setup the program
    PRE: true
    POST: IO ()
    SIDE EFFECTS: writes lines to the terminal
    EXAMPLES:
  -}
  main :: IO ()
  main = do
    home

  {-
    home
    PURPOSE: starts interface for the program
    PRE: true
    POST: IO ()
    SIDE EFFECTS: writes lines to the terminal
    EXAMPLES:
  -}
  home :: IO ()
  home = do
    clearScreen
    printLnCenter "Welcome to Three Alpacas Casino."
    printGameTable everyGame
    putStr " [q to quit]: "
    rawLine <- getLine
    let userAction = filterUserInput (map toUpper rawLine)
    if userAction == "Q" then do --quit casino
      putStrLn "Exiting Three Alpacas.."
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

  {-
    gameDoesntExist
    PURPOSE: to tell the user that no game exists for the input
    PRE: true
    POST: IO ()
    SIDE EFFECTS: writes lines to the terminal
    EXAMPLES:
  -}
  gameDoesntExist :: IO ()
  gameDoesntExist = do
    putStrLn "Game doesn't exist"
    home

  {-
    printGameTable games
    PURPOSE: to print the game table for the user.
    PRE: true
    POST: IO ()
    SIDE EFFECTS: writes lines to the terminal
    EXAMPLES:
  -}
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
    userActions
    PURPOSE: return a string with every valid number that are mapped to a game.
    PRE: true
    POST: string with valid user inputs
    SIDE EFFECTS: none
    EXAMPLES:  userActions == "Q"
  -}
  userActions :: String
  userActions = ['Q']

  {-
    userGames
    PURPOSE: return a string with every valid number that are mapped to a game.
    PRE: true
    POST: string containing valid game numbers
    SIDE EFFECTS: none
    EXAMPLES:  userGames == "12"
  -}
  userGames :: String
  userGames = map (head . show) [1 .. gameCount]

  {-
    validUserInput
    PURPOSE: combine actions and valid games into one string.
    PRE: true
    POST: string containg valid user inputs
    SIDE EFFECTS: none
    EXAMPLES: validUserInput = "Q12"
  -}
  validUserInput :: String
  validUserInput = userActions ++ userGames

  {-
    filterUserInput
    PURPOSE: remove elements that doesn't match valid user input.
    PRE: true
    POST: string with only valid user inputs
    SIDE EFFECTS: none
    EXAMPLES: filterUserInput "Quit" == "Q"
              filterUserInput "lmafsmlfas" == ""
  -}
  filterUserInput :: String -> String
  filterUserInput input = filter (\character -> elem character validUserInput) input

  {- TESTS -}
  testfilterUserInput1 = T.TestCase $ T.assertBool "filterUserInput1" ( filterUserInput "lmafsmlfas" == "")
  testfilterUserInput2 = T.TestCase $ T.assertBool "filterUserInput2" ( filterUserInput "Quit" == "Q")

  runtests = T.runTestTT $ T.TestList [testfilterUserInput1,
                                        testfilterUserInput2,
                                        testListHand,
                                        testListDeck,
                                        testListCard,
                                        P.testlistPlayer,
                                        BJ.testListBJ,
                                        P5.testListP5,
                                        testlistGame]
