module Card where

  import Test.HUnit

  {-
    REPRESENTATION CONVENTION: Suit Spades Clubs Diamonds Hearts represent the suit of each playingcard.
    REPRESENTATION INVARIANT: True
  -}
  data Suit = Spades
            | Clubs
            | Diamonds
            | Hearts

  {-
    REPRESENTATION CONVENTION: Value Other Int J Q K A respresent the value denomination of each card in the deck.
    REPRESENTATION INVARIANT: True
  -}
  data Value = Other Int
             | J
             | Q
             | K
             | A

  {-
    REPRESENTATION CONVENTION: Card Suit Value represent a playingcard where suit and value together create each card. InvisibleCard represent a card that
                                doesn't exist (a placeholder for nothing)
    REPRESENTATION INVARIANT: In Playingcard each card need to be represented with one Suit of Spades Clubs Diamond Hearts and
                              with one Value of Other Int J Q K A. InvisibleCard are always represented as an empty list.
  -}
  data PlayingCard = Card Suit Value
                   | InvisibleCard

  instance Show Suit where
    -- show Clubs = "♣"
    -- show Diamonds = "♦"
    -- show Hearts = "♥"
    -- show Spades = "♠"
    show Clubs = "C"
    show Diamonds = "D"
    show Hearts = "H"
    show Spades = "S"

  instance Show Value where
    show J = "J"
    show Q = "Q"
    show K = "K"
    show A = "A"
    show (Other value) = show(value)

  instance Show PlayingCard where
    show (Card suit value) = "[" ++ show(value) ++ show(suit) ++ "]"
    show InvisibleCard = "[]"

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
    fromEnum Spades = 0
    fromEnum Clubs = 1
    fromEnum Diamonds = 2
    fromEnum Hearts = 3

    toEnum enum =
      let
        s = ((ceiling (toRational(enum `mod` 52) / 13.0)))
      in
        if s == 1 then Spades
        else if s == 2 then Clubs
        else if s == 3 then Diamonds
        else Hearts

  instance Enum PlayingCard where
    fromEnum (Card suit value) = fromEnum(suit) * 13 + fromEnum(value)
    toEnum enum = (Card (toEnum enum::Suit) (toEnum (enum `mod` 13)::Value))

  instance Eq Value where
    (==) (Other valueA) (Other valueB) = (valueA == valueB)
    (==) J J = True
    (==) Q Q = True
    (==) K K = True
    (==) A A = True
    (==) _ _ = False

  instance Eq Suit where
    (==) Diamonds Diamonds = True
    (==) Clubs Clubs = True
    (==) Spades Spades = True
    (==) Hearts Hearts = True
    (==) _ _ = False

  instance Eq PlayingCard where
    (==) InvisibleCard InvisibleCard = True
    (==) (Card suitA valueA) (Card suitB valueB) = (valueA == valueB) && (suitA == suitB)

  {-
    TESTS
  -}
  testfromEnum1 = TestCase $ assertBool "testfromEnum" ((fromEnum (Card Spades A)) == 1)
  testfromEnum2 = TestCase $ assertBool "testfromEnum" ((fromEnum (Card Hearts K)) == 52)
  testtoEnum = TestCase $ assertBool "testtoEnum" ((toEnum 1) == (Card Spades A))
  testfromEnumAce = TestCase $ assertBool "testfromEnumAce" ((fromEnum (Card Spades A)) == 1 && fromEnum (Card Clubs A) == 14 && fromEnum (Card Diamonds A) == 27 && fromEnum (Card Hearts A) == 40)
  testiterateHearts = TestCase $ assertBool "testinterateHearts" (([(Card Hearts A)..(Card Hearts K)]) ==
        [Card Hearts A,Card Hearts (Other 2),
         Card Hearts (Other 3),Card Hearts (Other 4),
         Card Hearts (Other 5),Card Hearts (Other 6),
         Card Hearts (Other 7),Card Hearts (Other 8),
         Card Hearts (Other 9),Card Hearts (Other 10),
         Card Hearts J,Card Hearts Q,Card Hearts K])

  testListCard = TestList [testfromEnum1, testfromEnum2, testtoEnum, testfromEnumAce, testiterateHearts]
