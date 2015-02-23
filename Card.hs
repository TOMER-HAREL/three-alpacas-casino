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
    show (Card suit value) = show(suit) ++ " " ++ show(value)

data Suit = Clubs
          | Spades
          | Hearts
          | Diamonds

data Value = Other Int 
           | J
           | Q
           | K
           | A

data PlayingCard = Card Suit Value
