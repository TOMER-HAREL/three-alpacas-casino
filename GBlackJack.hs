module GBlackJack where

  import Game
  import Card

  BJCardValue :: PlayingCard -> Int
  BJCardValue (Card _ (Other value)) = value
  BJCardValue (Card _ A) = 11
  BJCardValue (Card _ _) = 10

  BJCalculateHand :: Hand -> Int
  BJCalculateHand (Hand []) = 0
  BJCalculateHand (Hand ((Card _ value):rest)) = (BJCardValue value) + BJCalculateHand rest

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
