module Card where

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

  data PlayingCard = Card Suit Value

  --instance
  instance Show Suit where
    show Clubs = "♣︎"
    show Diamonds = "♦︎"
    show Hearts = "❤︎"
    show Spades = "♠︎"

  instance Show Value where
    show J = "Jack"
    show Q = "Queen"
    show K = "King"
    show A = "Ace"
    show (Other value) = show(value)

  instance Show PlayingCard where
    show (Card suit value) = "[" ++ show(suit) ++ show(value) ++ "]"


  {-
    PURPOSE: to remove the hardcoded values of a card as we're designing for
             multiple card games. Creating a class let's us design our own
             functions for a certain card.
  -}
  class Card a where
    value :: a -> Int

  -- Eq
  instance Eq Suit where
    (==) Diamonds Diamonds = True
    (==) Clubs Clubs = True
    (==) Spades Spades = True
    (==) Hearts Hearts = True
    (==) _ _ = False

  -- instance Eq PlayingCard where
  --   (==) c1@(Card s1 _) c2@(Card s2 _) = (value c1 == value c2) && (s1 == s2)

  -- instance Ord PlayingCard where
  --   (<=) c1 c2 = (value c1 <= value c2)
