import Test.HUnit
import qualified Games.BlackJack as BJ
import qualified Games.GoFish as GF
import qualified Games.Poker as P5

import Card
import Hand
import Game
import Deck
import Player

main :: IO ()
main = do
  home

home :: IO ()
home = do
  putStrLn "Welcome to Playboy Casino."
  printGameTable everyGame
  line <- getLine
  putStrLn ("Picked game: " ++ show(line))
  home

{- TESTS -}
runtests = runTestTT $ TestList [testListHand, testListDeck, testListCard]
