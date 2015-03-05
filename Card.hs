module Card where

  import Test.HUnit

  {-
    REPRESENTATION CONVENTION: Suit Spades Clubs Diamonds Hearts represent the suit of each playingcard.
    REPRESENTATION INVARIANT: Suit denotes the suit of the card, straight forward.
  -}
  data Suit = Spades
            | Clubs
            | Diamonds
            | Hearts

  {-
    REPRESENTATION CONVENTION: Value Other Int J Q K A respresent the value denomination of each card in the deck.
    REPRESENTATION INVARIANT: Every value denotes a suit of a card, that would be
      J == Knight
      Q == Queen
      K == King
      A == Ace
      Other n == n
      (Other n), where n has to be in the interval of 2 to 10
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
      with one Value of Other Int J Q K A. InvisibleCard are always represented as a placeholder of nothing.
  -}
  data PlayingCard = Card Suit Value
                   | InvisibleCard

  {-
    show suit
    PURPOSE: override show for the datatype Suit, show it nicely when printed.
    PRE: the suits has to be defined in the instance.
    POST: a string that denotes the suit
    EXAMPLES: omitted to avoid spam
  -}
  instance Show Suit where
    -- show Clubs = "♣"
    -- show Diamonds = "♦"
    -- show Hearts = "♥"
    -- how Spades = "♠"
    show Clubs = "C"
    show Diamonds = "D"
    show Hearts = "H"
    show Spades = "S"

  {-
    show value
    PURPOSE: override show for the datatype Value, show it nicely when printed.
    PRE: the Value has to be defined in the instance.
    POST: a string that denotes the value
    EXAMPLES:
      show (Other 5) == "5"
      show K == "K"
  -}
  instance Show Value where
    show J = "J"
    show Q = "Q"
    show K = "K"
    show A = "A"
    show (Other value) = show(value)

  {-
    show card
    PURPOSE: override show for the datatype PlayingCard, show it as a card in ASCII
      when printed
    PRE: the Value and Suit for the card has to be defined in their own instances.
    POST: a string that denotes the card, value first and suit last
    EXAMPLES:
      show InvisibleCard == "[]"
      show (Card Diamonds (Other 5)) == "[5D]"
      show (Card Spades A) == "[AS]"
      show (Card Hearts K) == "[KH]"
  -}
  instance Show PlayingCard where
    show (Card suit value) = "[" ++ show(value) ++ show(suit) ++ "]"
    show InvisibleCard = "[]"

  {-
    WHY: to apply operations like [ .. ] for Value
    EXAMPLES:
      [A .. K] == [A,Other 2,Other 3,Other 4,Other 5,Other 6,Other 7,Other 8, Other 9,Other 10,J,Q,K]
      [A, (Other 3) .. K] == [A,Other 3,Other 5,Other 7,Other 9,J,K]
  -}
  instance Enum Value where
    {-
      fromEnum value
      PURPOSE: map a value to the respective number
      PRE: the value has to be defined in this instance
      POST: a number mapped to the Value.
      INVARIANT: (Other value) where value is the invariant and has to be in the correct interval, see Value CC
      EXAMPLES:
        fromEnum A == 1
        fromEnum (Other 5) == 5
    -}
    fromEnum A = 1
    fromEnum (Other value) = value
    fromEnum J = 11
    fromEnum Q = 12
    fromEnum K = 13

    {-
      toEnum n
      PURPOSE: map n to the respective value, the opposite of fromEnum Value
      PRE: true
      POST: a Value datatype that is mapped to n
      EXAMPLES:
        toEnum 0::Value == K
        toEnum 13::Value == K
        toEnum 6::Value == (Other 6)
    -}
    toEnum 1 = A
    toEnum 11 = J
    toEnum 12 = Q
    toEnum 13 = K
    toEnum 0 = K -- to fix the Enum
    toEnum value = (Other value)

  {-
    WHY: to apply operations like [ .. ] for Suit
    EXAMPLES:
      [Spades .. Hearts] == [Spades, Clubs, Diamonds, Hearts]
  -}
  instance Enum Suit where
    {-
      fromEnum suit
      PURPOSE: map a suit to the respective number
      PRE: the value has to be defined in this instance
      POST: a number mapped to the Value.
      EXAMPLES: omitted to avoid spam
    -}
    fromEnum Spades = 0
    fromEnum Clubs = 1
    fromEnum Diamonds = 2
    fromEnum Hearts = 3

    {-
      toEnum n
      PURPOSE: map n to the respective Suit, based on the cards in a deck. That would be
        13 numbers for every suit in the interval between 1 - 52
      PRE: n has to be a positive integer
      POST: a Suit datatype that is mapped to n
      INVARIANT: n can be anywhere in the range of positive Integers
      EXAMPLES:
        toEnum 1::Suit == Spades
        toEnum 14::Suit == Clubs
        toEnum 140::Suit == Diamonds
    -}
    toEnum enum =
      let
        s = ((ceiling (toRational(enum `mod` 52) / 13.0)))
      in
        if s == 1 then Spades
        else if s == 2 then Clubs
        else if s == 3 then Diamonds
        else Hearts

  {-
    WHY: to apply operations like [ .. ] for PlayingCard
    EXAMPLES:
      [(Card Spades A) .. (Card Hearts K)] returns a complete 52-card deck
      [(Card Spades A), (Card Spades (Other 3)) .. (Card Hearts K)] will return a deck of every odd card
      [(Card Spades A) ..] loops the cards til infinity
  -}
  instance Enum PlayingCard where

    {-
      fromEnum card
      PURPOSE: map a card to the respective number.
      PRE: Has to be a PlayingCard of the data constructor Card, InvisibleCard is invalid.
      POST: a number mapped to the PlayingCard, that would be the card index in a complete deck, 1 to 52
      EXAMPLES:
        fromEnum (Card Hearts K) == 52
        fromEnum (Card Spades A) == 1
    -}
    fromEnum (Card suit value) = fromEnum(suit) * 13 + fromEnum(value)

    {-
      toEnum n
      PURPOSE: map n to the respective PlayingCard (Card, InvisibleCard cannot be accessed).
      PRE: n has to be a positive integer
      POST: a PlayingCard datatype with the constructor Card that is mapped to n.
      INVARIANT: n can be anywhere in the range of positive Integers
      EXAMPLES:
        toEnum 1::PlayingCard == (Card Spades A)
        toEnum 14::PlayingCard == (Card Clubs A)
        toEnum 140::PlayingCard == (Card Diamonds (Other 10))
    -}
    toEnum enum = (Card (toEnum enum::Suit) (toEnum (enum `mod` 13)::Value))

  {-
    valueA == valueB
    PURPOSE: compare two values of data Value
    PRE: true
    POST: a boolean value that denotes if valueA is equal to valueB
    EXAMPLES:
      (J == J) == True
      ((Other 5) == (Other 5)) == True
      ((Other 5) == A) == False
  -}
  instance Eq Value where
    (==) (Other valueA) (Other valueB) = (valueA == valueB)
    (==) J J = True
    (==) Q Q = True
    (==) K K = True
    (==) A A = True
    (==) _ _ = False

  {-
    suitA == suitB
    PURPOSE: compare two values of data Suit
    PRE: true
    POST: a boolean value that denotes if suitA is equal to suitB
    EXAMPLES:
      (Diamonds == Diamonds) == True
      (Spades == Diamonds) == False
  -}
  instance Eq Suit where
    (==) Diamonds Diamonds = True
    (==) Clubs Clubs = True
    (==) Spades Spades = True
    (==) Hearts Hearts = True
    (==) _ _ = False

  {-
    cardA == cardB
    PURPOSE: compare two PlayingCards by suit and value
    PRE: true
    POST: a boolean value that denotes if the cards are equal based on suit and value
    EXAMPLES:
      (InvisibleCard == (Card Diamonds A)) == False
      ((Card Diamonds A) == (Card Diamonds A)) == True
      ((Card Diamonds A) == (Card Hearts A)) == False
  -}
  instance Eq PlayingCard where
    (==) InvisibleCard InvisibleCard = True
    (==) (Card suitA valueA) (Card suitB valueB) = (valueA == valueB) && (suitA == suitB)
    (==) _ _ = False


  {-
    suitA <= suitB
      suitA < suitB
      suitA > suitB
      suitA >= suitB
    PURPOSE: compare suits by value, larger, smaller, etc.
    PRE: the suits needs to be in the instance of Enum Suit.
    POST: a boolean value that denotes if the suits fulfill the operation.
    EXAMPLES:
      (Diamonds >= Diamonds) == True
      (Diamonds > Hearts) == False
      (Diamonds > Spades) == True
  -}
  instance Ord Suit where
    (<=) suitA suitB = (fromEnum suitA <= fromEnum suitB)

  {-
    valueA <= valueB
      valueA < valueB
      valueA > valueB
      valueA >= valueB
    PURPOSE: compare values by enum, larger, smaller, etc.
    PRE: the values needs to be in the instance of Enum Value.
    POST: a boolean enum that denotes if the values fulfill the operation.
    EXAMPLES:
      (A < K) == True
      (A >= K) == False
      (A < (Other 2)) == True
  -}
  instance Ord Value where
    (<=) valueA valueB = (fromEnum valueA <= fromEnum valueB)

  {-
    cardA <= cardB
      cardA < cardB
      cardA > cardB
      cardA >= cardB
    PURPOSE: compare cards by value, larger, smaller, etc.
    PRE: the cards needs to be in the instance of Enum PlayingCard.
    POST: a boolean value that denotes if the cards fulfill the operation.
    EXAMPLES:
      ((Card Diamonds A) > (Card Spades A)) == True
      ((Card Diamonds A) > (Card Diamonds A)) == False
      ((Card Diamonds A) >= (Card Diamonds A)) == False
  -}
  instance Ord PlayingCard where
    (<=) cardA cardB = fromEnum cardA <= fromEnum cardB

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
