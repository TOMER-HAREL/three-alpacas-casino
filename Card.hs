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
    valueOf InvisibleCard = (-1)
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
    show InvisibleCard = "[]"

  instance Eq Suit where
    (==) Diamonds Diamonds = True
    (==) Clubs Clubs = True
    (==) Spades Spades = True
    (==) Hearts Hearts = True
    (==) _ _ = False

  {-
    TODO: Match every card in every card game, different rules for each Game
      when matching cards for example. The game Black Jack doesn't mind the
      suits.
  -}
  instance Eq PlayingCard where
    (==) InvisibleCard InvisibleCard = True
    --UNDEFINED GAME
    (==) (Card suitA (Other valueA) None) (Card suitB (Other valueB) None) = (valueA == valueB) && (suitA == suitB)
    (==) (Card suitA J None) (Card suitB J None) = (suitA == suitB)
    (==) (Card suitA Q None) (Card suitB Q None) = (suitA == suitB)
    (==) (Card suitA K None) (Card suitB K None) = (suitA == suitB)
    (==) (Card suitA A None) (Card suitB A None) = (suitA == suitB)
    (==) (Card _ _ None) (Card _ _ None) = False
    -- BLACK JACK
    (==) cardA cardB = valueOf cardA == valueOf cardB

  instance Ord PlayingCard where
    (<=) InvisibleCard InvisibleCard = True
    (<=) cardA cardB = valueOf cardA <= valueOf cardB
