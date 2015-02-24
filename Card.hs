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
