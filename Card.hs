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
