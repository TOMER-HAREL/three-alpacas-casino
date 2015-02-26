module Card where

  import Game

  {- CLASSES -}

  class CardValue a where
    valueOf :: a -> Int

  {- DATA -}

  data Suit = Spades
            | Clubs
            | Diamonds
            | Hearts

  data Value = Other Int
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
    -- show Clubs = "♣︎"
    -- show Diamonds = "♦︎"
    -- show Hearts = "❤︎"
    -- show Spades = "♠︎"
    show Clubs = "C:"
    show Diamonds = "D:"
    show Hearts = "H:"
    show Spades = "S:"

  instance Show Value where
    show J = "J"
    show Q = "Q"
    show K = "K"
    show A = "A"
    show (Other value) = show(value)

  instance Enum Value where
    fromEnum A = 1
    fromEnum (Other value) = value
    fromEnum J = 11
    fromEnum Q = 12
    fromEnum K = 13

    toEnum 1 = A
    toEnum 11 = J
    toEnum 12 = Q
    toEnum 13 = K
    toEnum 0 = K -- to fix the Enum
    toEnum value = (Other value)

  instance Enum Suit where
    fromEnum Spades = 1
    fromEnum Clubs = 2
    fromEnum Diamonds = 3
    fromEnum Hearts = 4

    toEnum enum =
      let
        s = ((ceiling (toRational(enum `mod` 52) / 13.0)))
      in
        if s == 1 then Spades
        else if s == 2 then Clubs
        else if s == 3 then Diamonds
        else Hearts

  instance Enum PlayingCard where
    fromEnum (Card suit value _) = fromEnum(suit) * fromEnum(value)
    toEnum enum = (Card (toEnum enum::Suit) (toEnum (enum `mod` 13)::Value) BJ)

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
