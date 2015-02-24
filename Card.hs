{-use this as a base class for BJCard, PokerCard etc. -}
module Card (PlayingCard(..)) where

  import Game

  {-
    PURPOSE: to remove the hardcoded values of a card as we're designing for
             multiple card games. Creating a class let's us design our own
             functions for a certain card.
  -}
  class Card a where
    value :: a -> Int

  {-
    PURPOSE: Make the card more generic, that would be different values for
      different games.
    EXAMPLES:
      value (Card Diamonds A None) == 13
      value (Card Diamonds A BJ) == 11
  -}
  instance Card PlayingCard where
    -- Black Jack
    value (Card _ (Other value) BJ) = value
    value (Card _ A BJ) = 11
    value (Card _ _ BJ) = 10 --the rest of the cards
    -- Default
    value (Card _ (Other value) _) = value
    value (Card _ J _) = 10
    value (Card _ Q _) = 11
    value (Card _ K _) = 12
    value (Card _ A _) = 13 --or 1?

  -- Show
  instance Show Suit where
    show Clubs = "♣︎"
    show Diamonds = "♦︎"
    show Hearts = "❤︎"
    show Spades = "♠︎"

  instance Show Value where
    show (Other value) = show(value)
    show J = "Jack"
    show Q = "Queen"
    show K = "King"
    show A = "Ace"

  instance Show PlayingCard where
    show (Card suit value None) = "[" ++ show(suit) ++ show(value) ++ "]"
    show (Card suit value BJ) = "[BJ:" ++ show(suit) ++ show(value) ++ "]"

  -- Eq
  instance Eq Suit where
    (==) Diamonds Diamonds = True
    (==) Clubs Clubs = True
    (==) Spades Spades = True
    (==) Hearts Hearts = True
    (==) _ _ = False

  instance Eq PlayingCard where
    (==) c1@(Card s1 _ _) c2@(Card s2 _ _) = (value c1 == value c2) && (s1 == s2)

  instance Ord PlayingCard where
    (<=) c1 c2 = (value c1 <= value c2)

  -- data
  data Suit = Clubs
            | Spades
            | Hearts
            | Diamonds

  data Value = Other Int
             | J
             | Q
             | K
             | A

  data PlayingCard = Card Suit Value Game
