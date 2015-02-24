module GBlackJack where

  import Game
  import Card

  bjCardValue :: PlayingCard -> Int
  bjCardValue (Card _ (Other value)) = value
  bjCardValue (Card _ A) = 11
  bjCardValue (Card _ _) = 10

  bjCalculateHand :: Hand -> Int
  bjCalculateHand (Hand []) = 0
  bjCalculateHand (Hand ((Card _ value):rest)) = (bjCardValue value) + bjCalculateHand rest

  -- {-
  --   PURPOSE: Make the card more generic, that would be different values for
  --     different games.
  --   EXAMPLES:
  --     value (Card Diamonds A None) == 13
  --     value (Card Diamonds A BJ) == 11
  -- -}
  -- class Card a => BlackJack a where
  --   value :: a -> Int
  --
  -- data BJPlayingCard
  --
  -- instance Card FlexibleInstances => BlackJack PlayingCard where
  --   -- Black Jack
  --   value (Card _ (Other value)) = value
  --   value (Card _ A) = 11
  --   value (Card _ _) = 10 --the rest of the cards
