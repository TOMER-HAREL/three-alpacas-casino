module GGoFish where

  import Game
  import Card

  {-
    PURPOSE: Make the card more generic, that would be different values for
      different games.
    EXAMPLES:
      value (Card Diamonds A None) == 13
      value (Card Diamonds A BJ) == 11
  -}
  instance Card PlayingCard where
    -- Black Jack
    value (Card _ (Other value)) = value
    value (Card _ A) = 11
    value (Card _ _) = 210 --the rest of the cards
