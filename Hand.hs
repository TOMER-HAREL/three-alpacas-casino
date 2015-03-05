module Hand where

  import Test.HUnit
  import Data.List

  import Card

  {- DATA -}

  {-
    REPRESENTATION CONVENTION: Hand represents a hand of playing cards. EmptyHand represents a hand with no playing cards.
    REPRESENTATION INVARIANT: A hand cannot contain only invisible cards.
  -}
  data PlayingHand = Hand [PlayingCard]
                   | EmptyHand

  instance Show PlayingHand where
    show (Hand []) = ""
    show (Hand (card:[])) = show(card) ++ show(Hand [])
    show (Hand (card:rest)) = show(card) ++ " " ++ show(Hand rest)
    show EmptyHand = "Empty Hand"

  instance Eq PlayingHand where
    (==) (Hand cardsA) (Hand cardsB) = cardsA == cardsB
    (==) EmptyHand EmptyHand = True
    (==) _ _ = False
  {- FUNCTIONS -}

  {-
    emptyHand
    PURPOSE: Create empty playinghand
    PRE:  true
    POST: Creats a hand with no cards in it.
    SIDE EFFECTS: none
    EXAMPLES: emptyHand == ""
  -}
  emptyHand :: PlayingHand
  emptyHand = (Hand [])


  {-
    cardsFromHand hand
    PURPOSE: return a list of cards from a hand
    PRE:  true
    POST: list of cards in hand
    SIDE EFFECTS: none
    EXAMPLES: cardsFromHand (Hand [(Card Diamonds A),(Card Clubs Q)]) == [[AD],[QC]]
  -}
  cardsFromHand :: PlayingHand -> [PlayingCard]
  cardsFromHand (Hand cards) = cards

  {-
    sortHand hand
    PURPOSE: sort a hand
    PRE:  true
    POST: a hand with cards sorted their enums
    SIDE EFFECTS: none
    EXAMPLES: sortHand (Hand [(Card Diamonds A),(Card Clubs Q)]) == [QC] [AD]
  -}
  sortHand :: PlayingHand -> PlayingHand
  sortHand (Hand cards) = (Hand (sort cards))

  {-
    numberOfValuesInHand
    PURPOSE: count every card in hand that contains value of the supplied card
    PRE:  true
    POST: an int that represents the amount of cards thats equal to the value of the given value
    SIDE EFFECTS: none
    EXAMPLES: numberOfValuesInHand (Hand [(Card Diamonds A),(Card Hearts Q),(Card Clubs Q)]) Q == 2
  -}
  numberOfValuesInHand :: PlayingHand -> Value -> Int
  numberOfValuesInHand EmptyHand _ = 0
  numberOfValuesInHand hand needle =
    let
      numberOfValuesInHand' :: PlayingHand -> Value -> Int -> Int
      numberOfValuesInHand' (Hand []) _ acc = acc
      numberOfValuesInHand' (Hand ((Card _ value):rest)) needleValue acc
        | (value == needleValue) = numberOfValuesInHand' (Hand rest) needleValue (acc+1)
        | otherwise = numberOfValuesInHand' (Hand rest) needleValue acc
      numberOfValuesInHand' (Hand (InvisibleCard:rest)) needleValue acc = numberOfValuesInHand' (Hand rest) needleValue acc
    in
      numberOfValuesInHand' hand needle 0

  {-
    cardsInHand hand
    PURPOSE: count the cards in a hand.
    PRE:  true
    POST: the amount of cards in the hand
    SIDE EFFECTS: none
    EXAMPLES: cardsInHand (Hand [(Card Diamonds A), (Card Spades Q), (Card Clubs (Other 5))]) = 3

  -}

  cardsInHand :: PlayingHand -> Int
  cardsInHand (Hand cards) = length cards

  {-
    handContainsCard hand card
    PURPOSE: check if a hand contains a certain card.
    PRE:  true
    POST: bool that tells you if given card is in hand
    SIDE EFFECTS: none
    EXAMPLES: handContainsCard  (Hand [(Card Diamonds A), (Card Spades Q), (Card Clubs (Other 5))]) (Card Spades Q) = True
  -}

  handContainsCard :: PlayingHand -> PlayingCard -> Bool
  handContainsCard (Hand cards) card = elem card cards

  {-
    addCardToHand hand card
    PURPOSE: add a provided card to the hand in question, return the hand with
    the new card added.
    PRE: true
    POST: a hand with the provided card
    SIDE EFFECTS: none
    EXAMPLES: addCardToHand (Hand [(Card Diamonds A)]) (Card Clubs K) == [KC] [AD]
  -}

  addCardToHand :: PlayingHand -> PlayingCard -> PlayingHand
  addCardToHand EmptyHand card = (Hand [card])
  addCardToHand (Hand cards) card = (Hand (card:cards))


  {-
    addCardsToHand han cards
    PURPOSE: add multiple cards to a hand
    PRE: true
    POST: a hand with mulitple cards added
    SIDE EFFECTS: none
    EXAMPLES:
  -}
  addCardsToHand :: PlayingHand -> [PlayingCard] -> PlayingHand
  addCardsToHand EmptyHand cardsToAdd = (Hand (cardsToAdd))
  addCardsToHand (Hand cards) cardsToAdd = (Hand (cardsToAdd ++ cards))

  {-
    cardAtPosition hand position
    PURPOSE: Return the card at the supplied position
    PRE: must provide an index inside the list of cards
    POST: the card at the given index
    SIDE EFFECTS: none
    EXAMPLES: cardAtPosition (Hand [(Card Diamonds A), (Card Spades (Other 10))]) 1 == [10S]
  -}

  cardAtPosition :: PlayingHand -> Int -> PlayingCard
  cardAtPosition (Hand cards) position = cards !! position

  {-
    removeCardAtPosition hand position
    PURPOSE:  Remove card at position and return the new hand.
    PRE:  must provide an index inside the list of cards
    POST: a playinghand with a card removed from the given index
    SIDE EFFECTS: index
    EXAMPLES: removeCardAtPosition  (Hand [(Card Diamonds A), (Card Spades (Other 10))]) 1 == [AD]
  -}

  removeCardAtPosition :: PlayingHand -> Int -> PlayingHand
  removeCardAtPosition hand@(Hand cards) position  = (Hand (delete (cardAtPosition hand position) cards))

  {- TESTS -}

  testHand :: PlayingHand
  testHand = (Hand [(Card Diamonds A), (Card Spades (Other 5)), (Card Clubs K), (Card Diamonds (Other 2))])

  testemptyHand = TestCase $ assertBool "emptyHand" (emptyHand == (Hand []))
  testcardsFromHand = TestCase $ assertBool "cardsFromHand" (cardsFromHand testHand == [(Card Diamonds A),(Card Spades (Other 5)),(Card Clubs K),(Card Diamonds (Other 2))])
  testsortHand = TestCase $ assertBool "sortHand" (sortHand testHand == (Hand [(Card Spades (Other 5)),(Card Clubs K),(Card Diamonds A),(Card Diamonds (Other 2))]))
  testnumberOfValuesInHand = TestCase $ assertBool "numberOfValuesInHand" (numberOfValuesInHand testHand K == 1)
  testcardsInHand = TestCase $ assertBool "cardsInHand" (cardsInHand testHand == 4)
  testHandContainsCard = TestCase $ assertBool "HandContainsCard" (handContainsCard testHand (Card Diamonds A) == True)
  testCardAtPosition = TestCase $ assertBool "CardAtPosition" ((cardAtPosition testHand 1) == (Card Spades (Other 5)))
  testRemoveCardAtPosition = TestCase $ assertBool "RemoveCardAtPosition" ((removeCardAtPosition testHand 1) == (Hand [(Card Diamonds A), (Card Clubs K), (Card Diamonds (Other 2))]))
  testAddCardToHand = TestCase $ assertBool "addCardToHand" ((addCardToHand testHand (Card Diamonds J)) == (Hand [(Card Diamonds J), (Card Diamonds A), (Card Spades (Other 5)), (Card Clubs K), (Card Diamonds (Other 2))]))
  testaddCardsToHand = TestCase $ assertBool "addCardsToHand" (addCardsToHand (Hand [(Card Diamonds A), (Card Spades (Other 5))]) [(Card Clubs K),(Card Diamonds (Other 2))] == (Hand [(Card Clubs K),(Card Diamonds (Other 2)),(Card Diamonds A), (Card Spades (Other 5))]))

  testListHand = TestList [testemptyHand,
                          testcardsFromHand,
                          testsortHand,
                          testnumberOfValuesInHand,
                          testcardsInHand,
                          testCardAtPosition,
                          testRemoveCardAtPosition,
                          testAddCardToHand,
                          testHandContainsCard,
                          testaddCardsToHand]
