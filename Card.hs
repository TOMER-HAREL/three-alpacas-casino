module Card where

  import Test.HUnit

  {- DATA -}

  {-  REPRESENTATION CONVENTION: ... description of how the datatype represents data ...
      REPRESENTATION INVARIANT:  ... requirements on elements of the datatype that the code preserves at all times ...
     -}

  data Suit = Spades
            | Clubs
            | Diamonds
            | Hearts

  {-  REPRESENTATION CONVENTION: ... description of how the datatype represents data ...
      REPRESENTATION INVARIANT:  ... requirements on elements of the datatype that the code preserves at all times ...
     -}

  data Value = Other Int
             | J
             | Q
             | K
             | A

 {-  REPRESENTATION CONVENTION: ... description of how the datatype represents data ...
     REPRESENTATION INVARIANT:  ... requirements on elements of the datatype that the code preserves at all times ...
    -}

  data PlayingCard = Card Suit Value
                   | InvisibleCard

  instance Show Suit where
    show Clubs = "♣︎"
    show Diamonds = "♦︎"
    show Hearts = "❤︎"
    show Spades = "♠︎"
    -- show Clubs = "C"
    -- show Diamonds = "D"
    -- show Hearts = "H"
    -- show Spades = "S"

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

  instance Show PlayingCard where
    show (Card suit value) = "[" ++ show(value) ++ show(suit) ++ "]"
    show InvisibleCard = "[]"

  instance Eq Suit where
    (==) Diamonds Diamonds = True
    (==) Clubs Clubs = True
    (==) Spades Spades = True
    (==) Hearts Hearts = True
    (==) _ _ = False

  {-
    PURPOSE: match cards
  -}
  instance Eq PlayingCard where
    (==) InvisibleCard InvisibleCard = True
    (==) (Card suitA (Other valueA)) (Card suitB (Other valueB)) = (valueA == valueB) && (suitA == suitB)
    (==) (Card suitA J) (Card suitB J) = (suitA == suitB)
    (==) (Card suitA Q) (Card suitB Q) = (suitA == suitB)
    (==) (Card suitA K) (Card suitB K) = (suitA == suitB)
    (==) (Card suitA A) (Card suitB A) = (suitA == suitB)
    (==) (Card _ _) (Card _ _) = False

  {- TESTS -}

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
