module Hand where

  import Test.HUnit
  import Data.List

  import Card
  import Game

  {- CLASSES -}

  class HandValue a where
    sumOfHand :: a -> Int
    numberOfCards :: a -> Int
    maximumNumberOfCards :: a -> NumberOfCards


  data NumberOfCards = Limit Int | NoLimit

  {- DATA -}

  data PlayingHand = Hand [PlayingCard] Game
                   | EmptyHand

  {- INSTANCES -}

  instance Show PlayingHand where
    show (Hand [] _) = []
    show (Hand (card:xs) game) = show(card) ++ " " ++ show(Hand xs game)

  instance HandValue PlayingHand where
    sumOfHand (Hand [] _) = 0
    sumOfHand (Hand (card:rest) game) = (valueOf card) + (sumOfHand (Hand rest game))

    {- numberOfCards hand
   PURPOSE:  count the cards in hand
   PRE:  ... pre-condition on the arguments ...
   POST: ... post-condition on the result, in terms of the arguments ...
   SIDE EFFECTS: ... if any, including exceptions ...
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
   -}

    numberOfCards (Hand cards BJ) = length cards

    {-
      maximumNumberOfCards hand
      PURPOSE: Return the maximum number of cards you're allowed to have in a hand
      PRE:  ... pre-condition on the arguments ...
      POST: ... post-condition on the result, in terms of the arguments ...
      SIDE EFFECTS: ... if any, including exceptions ...
      EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
    -}

    maximumNumberOfCards (Hand cards BJ) = NoLimit
    maximumNumberOfCards (Hand cards GF) = NoLimit
    maximumNumberOfCards (Hand cards TX) = Limit 5

  instance Eq PlayingHand where
    (==) (Hand cardsA _) (Hand cardsB _) = cardsA == cardsB

  {-
    TODO
    PURPOSE: to order cards in a playingHand
    HINT:
  -}
  instance Ord PlayingHand where
    (<=) (Hand cardsA _) (Hand cardsB _) = undefined

  {- FUNCTIONS -}

  {-
    TODO
    PURPOSE: add a provided card to the hand in question, return the hand with
      the new card added.
  -}
  addCardToHand :: PlayingHand -> PlayingCard -> PlayingHand
  addCardToHand hand card = undefined

  {-
   cardAtPosition hand position
   PURPOSE: Return the card at the supplied position
   PRE:  ... pre-condition on the arguments ...
   POST: ... post-condition on the result, in terms of the arguments ...
   SIDE EFFECTS: ... if any, including exceptions ...
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
   -}

  cardAtPosition :: PlayingHand -> Int -> PlayingCard
  cardAtPosition (Hand cards _) position = cards !! position

    {- removeCardAtPosition hand position
   PURPOSE:  Remove the card and return the new hand.
   PRE:  ... pre-condition on the arguments ...
   POST: ... post-condition on the result, in terms of the arguments ...
   SIDE EFFECTS: ... if any, including exceptions ...
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
  -}

  removeCardAtPosition :: PlayingHand -> Int -> PlayingHand
  removeCardAtPosition hand@(Hand cards game) position  = (Hand (delete (cardAtPosition hand position) cards) game)

  {- TESTS -}
  testHand :: PlayingHand
  testHand = (Hand [(Card Diamonds A BJ), (Card Spades (Other 5) BJ), (Card Clubs K BJ), (Card Diamonds (Other 2) BJ)] BJ)

  testCardAtPosition = TestCase $ assertBool "CardAtPosition" ((cardAtPosition testHand 1) == (Card Spades (Other 5) BJ))
  testRemoveCardAtPosition = TestCase $ assertBool "RemoveCardAtPosition" ((removeCardAtPosition testHand 1) == (Hand [(Card Diamonds A BJ), (Card Clubs K BJ), (Card Diamonds (Other 2) BJ)] BJ))
  testAddCardToHand = TestCase $ assertBool "addCardToHand" ((addCardToHand testHand (Card Diamonds J BJ)) == (Hand [(Card Diamonds J BJ), (Card Diamonds A BJ), (Card Spades (Other 5) BJ), (Card Clubs K BJ), (Card Diamonds (Other 2) BJ)] BJ))

  testListHand = TestList [testCardAtPosition, testRemoveCardAtPosition, testAddCardToHand]
