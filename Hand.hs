module Hand where

  import Test.HUnit
  import Data.List

  import Card
  import Game

  {- CLASSES -}

  class HandValue a where
    sumOfHand :: a -> Int
    numberOfCards :: a -> Int
    maximumNumberOfCards :: a -> Int

  {- DATA -}

  data PlayingHand = Hand [PlayingCard] Game

  {- INSTANCES -}

  instance Show PlayingHand where
    show (Hand [] _) = []
    show (Hand (card:xs) game) = show(card) ++ " " ++ show(Hand xs game)

  instance HandValue PlayingHand where
    sumOfHand (Hand [] _) = 0
    sumOfHand (Hand (card:rest) game) = (valueOf card) + (sumOfHand (Hand rest game))

    {-
      TODO
      PURPOSE: Count the cards in hand
      HINT: length
    -}
    numberOfCards (Hand cards BJ) = undefined
    {-
      TODO
      PURPOSE: Return the maximum number of cards you're allowed to have in a hand
    -}
    maximumNumberOfCards (Hand cards BJ) = undefined

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
    PURPOSE: add a provided card to the hand in question, return the hand with
      the new card added.
  -}
  addCardToHand :: PlayingHand -> PlayingCard -> PlayingHand
  addCardToHand hand card = undefined

  {-
    PURPOSE: Return the card at the supplied position
  -}
  cardAtPosition :: PlayingHand -> Int -> PlayingCard
  cardAtPosition (Hand cards _) position = cards !! position

  {-
    PURPOSE: Remove the card and return the new hand.
  -}
  removeCardAtPosition :: PlayingHand -> Int -> PlayingHand
  removeCardAtPosition hand@(Hand cards game) position  = (Hand (delete (cardAtPosition hand position) cards) game)

  {- TESTS -}
  testHand :: PlayingHand
  testHand = (Hand [(Card Diamonds A BJ), (Card Spades (Other 5) BJ), (Card Clubs K BJ), (Card Diamonds (Other 2) BJ)] BJ)

  testCardAtPosition = TestCase $ assertBool "CardAtPosition" ((cardAtPosition testHand 1) == (Card Spades (Other 5) BJ))
  testRemoveCardAtPosition = TestCase $ assertBool "RemoveCardAtPosition" ((removeCardAtPosition testHand 1) == (Hand [(Card Diamonds A BJ), (Card Clubs K BJ), (Card Diamonds (Other 2) BJ)] BJ))

  testListHand = TestList [testCardAtPosition, testRemoveCardAtPosition]
