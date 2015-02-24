module Card where

  import Game

  {- CLASSES -}

  class CardValue a where
    valueOf :: a -> Int

  {- DATA -}

  data Suit = Clubs
            | Spades
            | Hearts
            | Diamonds

  data Value = Other Int
             | LowHigh (Value, Value)
             | J
             | Q
             | K
             | A

  data PlayingCard = Card Suit Value Game
                   | InvisibleCard

  {- INSTANCES -}

  instance CardValue PlayingCard where
    --BLACK JACK
    valueOf (Card _ (Other value) BJ) = value
    valueOf (Card _ A BJ) = 11
    valueOf (Card _ _ BJ) = 10
    --GO FISH
    valueOf (Card _ _ GF) = 0

  instance Show Suit where
    show Clubs = "♣︎"
    show Diamonds = "♦︎"
    show Hearts = "❤︎"
    show Spades = "♠︎"

  instance Show Value where
    show J = "J"
    show Q = "Q"
    show K = "K"
    show A = "A"
    show (Other value) = show(value)

  instance Show PlayingCard where
    show (Card suit value None) = "U:[" ++ show(value) ++ show(suit) ++ "]"
    show (Card suit value _) = "[" ++ show(value) ++ show(suit) ++ "]"

  instance Eq Suit where
    (==) Diamonds Diamonds = True
    (==) Clubs Clubs = True
    (==) Spades Spades = True
    (==) Hearts Hearts = True
    (==) _ _ = False

  instance Eq PlayingCard where
    (==) (Card suitA (Other valueA) None) (Card suitB (Other valueB) None) = (valueA == valueB) && (suitA == suitB)
    (==) (Card suitA J None) (Card suitB J None) = (suitA == suitB)
    (==) (Card suitA Q None) (Card suitB Q None) = (suitA == suitB)
    (==) (Card suitA K None) (Card suitB K None) = (suitA == suitB)
    (==) (Card suitA A None) (Card suitB A None) = (suitA == suitB)
    (==) (Card _ _ None) (Card _ _ None) = False

  instance Ord PlayingCard where
    
