module GBlackJack where

  import Game
  import Card
  import Hand

  class BJCard a where
    value :: a -> Int

  instance BJCard PlayingCard where
    value (Card _ (Other value)) = value
    value (Card _ A) = 11
    value (Card _ _) = 10
