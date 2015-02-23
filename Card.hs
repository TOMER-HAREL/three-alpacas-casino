module Card (PlayingCard(Card), Suit(..), Value(..), cardValue) where

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
      show (Card suit value) = "[" ++ show(suit) ++ show(value) ++ "]"

  instance Eq PlayingCard where
      (==) (Card _ (Other value)) (Card _ (Other value2)) = (value == value2)
      (==) (Card _ A) (Card _ A) = True
      (==) (Card _ K) (Card _ K) = True
      (==) (Card _ Q) (Card _ Q) = True
      (==) (Card _ J) (Card _ J) = True
      (==) (Card _ _) (Card _ _) = False


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

  {-
    cardValue card
    PURPOSE: return the value of the card
    PRE: true
    POST: If (-1) is returned it's an invalid card value, otherwise it'll return
          a number that denotes the value of the card supplied.
    EXAMPLES:
          cardValue (Card Clubs (Other 10)) == 10
          cardValue (Card Clubs (Other 1337)) == (-1)
          cardValue (Card Clubs A) == 11
  -}
  cardValue :: PlayingCard -> Int
  cardValue (Card _ (Other value))
    | (value >= 1 && value <= 10) = value
    | otherwise = (-1)
  cardValue (Card _ A) = 11
  cardValue (Card _ value) = 10
