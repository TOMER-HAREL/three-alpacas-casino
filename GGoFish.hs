module GGoFish where

  import Game
  import Card

  class GFCard a where
    value :: a -> Int

  instance GFCard PlayingCard where
    value _ = 0
