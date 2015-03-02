module Hand where

  import Test.HUnit
  import Data.List

  import Card

  {-  REPRESENTATION CONVENTION: ... description of how the datatype represents data ...
      REPRESENTATION INVARIANT:  ... requirements on elements of the datatype that the code preserves at all times ...
   -}

  data PlayingHand = Hand [PlayingCard]
                   | EmptyHand

  {- INSTANCES -}

  instance Show PlayingHand where
    show (Hand []) = []
    show (Hand (card:xs)) = show(card) ++ " " ++ show(Hand xs)

  instance Eq PlayingHand where
    (==) (Hand cardsA) (Hand cardsB) = cardsA == cardsB


  {- FUNCTIONS -}

  {-
    PURPOSE: Create empty playinghand
  -}
  emptyHand :: PlayingHand
  emptyHand = (Hand [])

  {-
      addCardToHand hand card
      PURPOSE: add a provided card to the hand in question, return the hand with
      the new card added.
      PRE:  ... pre-condition on the arguments ...
      POST: ... post-condition on the result, in terms of the arguments ...
      SIDE EFFECTS: ... if any, including exceptions ...
      EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
  -}
  addCardToHand :: PlayingHand -> PlayingCard -> PlayingHand
  addCardToHand (Hand cards) card = (Hand (card:cards))

  {-
      cardAtPosition hand position
      PURPOSE: Return the card at the supplied position
      PRE:  ... pre-condition on the arguments ...
      POST: ... post-condition on the result, in terms of the arguments ...
      SIDE EFFECTS: ... if any, including exceptions ...
      EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
   -}
  cardAtPosition :: PlayingHand -> Int -> PlayingCard
  cardAtPosition (Hand cards) position = cards !! position

  {-
    PURPOSE: alias for cardAtPosition
  -}
  (!!!) :: PlayingHand -> Int -> PlayingCard
  (!!!) = cardAtPosition

  {-
    PURPOSE: return the number of cards in a hand
  -}
  numberOfCards :: PlayingHand -> Int
  numberOfCards (Hand cards) = length cards

  {-  removeCardAtPosition hand position
      PURPOSE:  Remove the card and return the new hand.
      PRE:  ... pre-condition on the arguments ...
      POST: ... post-condition on the result, in terms of the arguments ...
      SIDE EFFECTS: ... if any, including exceptions ...
      EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
  -}
  removeCardAtPosition :: PlayingHand -> Int -> PlayingHand
  removeCardAtPosition hand@(Hand cards) position  = (Hand (delete (cardAtPosition hand position) cards))

  -- {- TESTS -}
  testHand :: PlayingHand
  testHand = (Hand [(Card Diamonds A), (Card Spades (Other 5)), (Card Clubs K), (Card Diamonds (Other 2))])

  testCardAtPosition = TestCase $ assertBool "CardAtPosition" ((cardAtPosition testHand 1) == (Card Spades (Other 5)))
  testRemoveCardAtPosition = TestCase $ assertBool "RemoveCardAtPosition" ((removeCardAtPosition testHand 1) == (Hand [(Card Diamonds A), (Card Clubs K), (Card Diamonds (Other 2))]))
  testAddCardToHand = TestCase $ assertBool "addCardToHand" ((addCardToHand testHand (Card Diamonds J)) == (Hand [(Card Diamonds J), (Card Diamonds A), (Card Spades (Other 5)), (Card Clubs K), (Card Diamonds (Other 2))]))

  testListHand = TestList [testCardAtPosition, testRemoveCardAtPosition, testAddCardToHand]
