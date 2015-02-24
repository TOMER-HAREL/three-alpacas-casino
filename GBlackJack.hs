module GBlackJack where

  import Game
  import Card
  import Hand

  bjCardValue :: PlayingCard -> Int
  bjCardValue (Card _ (Other value)) = value
  bjCardValue (Card _ A) = 11
  bjCardValue (Card _ _) = 10

  bjCalculateHand :: PlayingHand -> Int
  bjCalculateHand (Hand cards) = bjCalculateHand_aux cards

  bjCalculateHand_aux :: [PlayingCard] -> Int
  bjCalculateHand_aux [] = 0
  bjCalculateHand_aux (card:rest) = (bjCardValue card) + bjCalculateHand_aux rest

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
