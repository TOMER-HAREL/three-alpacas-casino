module Games.GoFish where

  import Game
  import Card
  import Hand
  import Player
  import Deck

  main :: IO ()
  main = do
    putStrLn ("Welcome to " ++ show(GF))

  createDeck :: PlayingDeck
  createDeck = shuffleDeck (createEmptyDeck)

  dealCards :: PlayingDeck -> GamePlayer -> GamePlayer
  dealCards EmptyDeck _ = undefined
  dealCards deck player = undefined
