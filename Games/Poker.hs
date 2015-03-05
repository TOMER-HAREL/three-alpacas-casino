module Games.Poker where

    import qualified Test.HUnit as T
    import Game
    import Card
    import Hand
    import Player
    import Deck
    import Data.List

    instance GameValue PlayingCard where
      valueOf (Card _ (Other value)) = value
      valueOf (Card _ J) = 11
      valueOf (Card _ Q) = 12
      valueOf (Card _ K) = 13
      valueOf (Card _ A) = 14

    instance GameValue PlayingHand where
        valueOf (Hand []) = 0
        valueOf (Hand (card:rest)) = (valueOf card) + (valueOf (Hand rest))

    instance GameValue PlayingDeck where
        valueOf (Deck []) = 0
        valueOf (Deck (card:rest)) = (valueOf card) + (valueOf (Deck rest))

    {-
      PURPOSE: main function to fire it all up.
    -}
    main :: IO ()
    main = do
      putStrLn ("Welcome to " ++ show(P5))
      -- setupGameState <- setupPhase
      -- gamePhase setupGameState
      return ()


    {-
      isRoyalStraigtFlush hand
      PURPOSE: see if hand is a royal straight flush
      PRE:  true
      POST: bool that tells you if hand is royal straight flush
      SIDE EFFECTS: none
      EXAMPLES: isRoyalStraightFlush (Hand [(Card Diamonds J), (Card Hearts K), (Card Diamonds A), (Card Diamonds Q), (Card Diamonds (Other 10))])) == False
                isRoyalStraightFlush (Hand [(Card Diamonds K), (Card Diamonds Q), (Card Diamonds A), (Card Diamonds J), (Card Diamonds (Other 10))])) == True
    -}
    isRoyalStraightFlush:: PlayingHand -> Bool
    isRoyalStraightFlush hand = let
                                numbers = handValues hand
                                in
                                  elem 14 numbers && (isStraightFlush hand)

    {-
      isStraightFlush hand
      PURPOSE: see if hand is a straight flush
      PRE:  true
      POST: bool that tells you if hand is straight flush
      SIDE EFFECTS: none
      EXAMPLES: isStraightFlush (Hand [(Card Diamonds (Other 4)), (Card Diamonds (Other 7)), (Card Diamonds (Other 6)), (Card Diamonds (Other 3)), (Card Diamonds (Other 5))])) == True
    -}
    isStraightFlush :: PlayingHand -> Bool
    isStraightFlush hand = (isFlush hand) && (isStraight hand)

    {-
      isFourOfAKind
      PURPOSE: see if hand is got four of the same card
      PRE:  true
      POST: bool that tells you if hand got four of the same card
      SIDE EFFECTS: none
      EXAMPLES: isFourOfAKind (Hand [(Card Clubs (Other 10)), (Card Diamonds (Other 7)), (Card Hearts (Other 10)), (Card Diamonds (Other 10)), (Card Spades (Other 10))])) == True
    -}
    isFourOfAKind :: PlayingHand -> Bool
    isFourOfAKind hand = let
                          numbers = map (\value -> numberOfValuesInHand hand value) [A .. K]
                          in
                            elem 4 numbers
    {-
      isFullHouse hand
      PURPOSE: see if hand is a full house
      PRE:  true
      POST: bool that tells you if hand is a full house
      SIDE EFFECTS: none
      EXAMPLES: isFullHouse (Hand [(Card Clubs (Other 6)), (Card Diamonds (Other 6)), (Card Hearts K), (Card Diamonds (Other 6)), (Card Spades K )])) == True
    -}
    isFullHouse :: PlayingHand -> Bool
    isFullHouse hand = (isThreeOfAKind hand) && (isPair hand)

    {-
      isflush hand
      PURPOSE: see if hand is a flush
      PRE:  true
      POST: bool that tells you if hand is a flush
      SIDE EFFECTS: none
      EXAMPLES: isFlush (Hand [(Card Clubs (Other 6)), (Card Clubs (Other 7)), (Card Clubs (Other 9)), (Card Clubs (Other 8)), (Card Clubs (Other 5))])) == True
    -}
    isFlush :: PlayingHand -> Bool
    isFlush (Hand ((Card suitA _):(Card suitB _):(Card suitC _):(Card suitD _):(Card suitE _):rest))
                                                                                            = suitA == suitB
                                                                                            && suitA == suitC
                                                                                            && suitA == suitD
                                                                                            && suitA == suitE
    isFlush _ = False

    {-
    isStraight hand
    PURPOSE: check if the card in in numerical order.
    PRE:  true
    POST: Returns a bool that tells you if the hand contains isStraight
    SIDE EFFECTS: none
    EXAMPLES: (isStraight (Hand [(Card Diamonds (Other 3)), (Card Hearts (Other 4)), (Card Clubs (Other 5)), (Card Spades (Other 6)), (Card Diamonds (Other 7))])) == True)
    -}
    isStraight :: PlayingHand -> Bool
    isStraight hand = let
                    numbers = handValues hand
                  in
                    (numbers !! 0 == numbers !! 1 - 1) && (numbers !! 0 == numbers !! 2 - 2) && (numbers !! 0 == numbers !! 3 - 3) && (numbers !! 0 == numbers !! 4 - 4)
    {-
        handValues hand
        PURPOSE: Calculate the value of each card and sort it from low-to-high.
        PRE:  true
        POST: Returns a list that is storted from low-to-high
        SIDE EFFECTS: none
        EXAMPLES: (handValues (Hand [(Card Diamonds (Other 5)), (Card Diamonds J), (Card Diamonds A), (Card Diamonds (Other 5)), (Card Diamonds (Other 10))])) == [5,5,10,11,14]
    -}
    handValues :: PlayingHand -> [Int]
    handValues (Hand cards) = sort $ map (\card -> valueOf card) cards


    {-
        isThreeOfAKind hand
        PURPOSE: check if the hand contains 3 cards of the same value
        PRE:  true
        POST: Returns a bool that tells you if the hand contains isThreeOfAKind or not
        SIDE EFFECTS: none
        (isThreeOfAKind (Hand [(Card Diamonds (Other 5)), (Card Hearts (Other 5)), (Card Clubs (Other 5)), (Card Spades K), (Card Diamonds (Other 7))])) == True)
    -}
    isThreeOfAKind :: PlayingHand -> Bool
    isThreeOfAKind hand = let
                            numbers = map (\value -> numberOfValuesInHand hand value) [A .. K]
                            in
                              elem 3 numbers

    {-
          isTwoPair hand
          PURPOSE: check if the hands contains two pairs
          PRE:  true
          POST: Returns a bool that tells you if there appears isTwoPair
          SIDE EFFECTS: none
          (isTwoPair (Hand [(Card Diamonds (Other 5)), (Card Hearts (Other 5)), (Card Clubs (Other 3)), (Card Spades K), (Card Diamonds (Other 3))])) == True)
     -}
    isTwoPair :: PlayingHand -> Bool
    isTwoPair hand = let
                       numbers = map (\value -> numberOfValuesInHand hand value) [A .. K]
                       numbersOfCombos = map (\n -> length n) (group $ sort numbers)
                       in
                         numbersOfCombos !! 1 == 2
    {-
        isPair hand
        PURPOSE: check if there is any pair in hand
        PRE:  true
        POST: Returns a bool that tells you if there appears isPair in the hand or not.
        SIDE EFFECTS: none
        EXAMPLES: isPair (Hand [(Card Diamonds (Other 5)), (Card Hearts (Other 5)), (Card Clubs (Other 3)), (Card Spades K), (Card Diamonds (Other 7))])) == True)
    -}
    isPair :: PlayingHand -> Bool
    isPair hand = let
                numbers = map (\value -> numberOfValuesInHand hand value) [A .. K]
                in
                  elem 2 numbers


        {- TESTS -}

    testisPair = T.TestCase $ T.assertBool "testisPair" ((isPair (Hand [(Card Diamonds (Other 5)), (Card Hearts (Other 5)), (Card Clubs (Other 3)), (Card Spades K), (Card Diamonds (Other 7))])) == True)
    testisPair2 = T.TestCase $ T.assertBool "testisPair2" ((isPair (Hand [(Card Diamonds (Other 5)), (Card Hearts (Other 5)), (Card Clubs (Other 5)), (Card Spades K), (Card Diamonds (Other 7))])) == False)
    testisTwoPair1 = T.TestCase $ T.assertBool "testisTwoPair1" ((isTwoPair (Hand [(Card Diamonds (Other 5)), (Card Hearts (Other 5)), (Card Clubs (Other 3)), (Card Spades K), (Card Diamonds (Other 3))])) == True)
    testisTwoPair2 = T.TestCase $ T.assertBool "testisTwoPair2" ((isTwoPair (Hand [(Card Diamonds (Other 5)), (Card Hearts (Other 5)), (Card Clubs (Other 5)), (Card Spades K), (Card Diamonds (Other 7))])) == False)
    testisThreeOfAKind = T.TestCase $ T.assertBool "testisThreeOfAKind" ((isThreeOfAKind (Hand [(Card Diamonds (Other 5)), (Card Hearts (Other 5)), (Card Clubs (Other 5)), (Card Spades K), (Card Diamonds (Other 7))])) == True)
    testisStraight = T.TestCase $ T.assertBool "testisStraight" ((isStraight (Hand [(Card Diamonds (Other 3)), (Card Hearts (Other 4)), (Card Clubs (Other 5)), (Card Spades (Other 6)), (Card Diamonds (Other 7))])) == True)
    testisStraight2 = T.TestCase $ T.assertBool "testisStraight1" ((isStraight (Hand [(Card Diamonds K), (Card Hearts (Other 2)), (Card Clubs (Other 3)), (Card Spades (Other 4)), (Card Diamonds (Other 5))])) == False)
    testisFlush1 = T.TestCase $ T.assertBool "testisFlush1" ((isFlush (Hand [(Card Clubs (Other 6)), (Card Clubs (Other 7)), (Card Clubs (Other 9)), (Card Clubs (Other 8)), (Card Clubs (Other 5))])) == True)
    testisFlush2 = T.TestCase $ T.assertBool "testisFlush2" ((isFlush (Hand [(Card Diamonds (Other 6)), (Card Diamonds (Other 7)), (Card Diamonds (Other 9)), (Card Diamonds (Other 8)), (Card Diamonds (Other 5))])) == True)
    testisFullHouse = T.TestCase $ T.assertBool "testisFullHouse" ((isFullHouse (Hand [(Card Clubs (Other 6)), (Card Diamonds (Other 6)), (Card Hearts K), (Card Diamonds (Other 6)), (Card Spades K )])) == True)
    testisFourOfAKind = T.TestCase $ T.assertBool "testisFourOfAKind" ((isFourOfAKind (Hand [(Card Clubs (Other 10)), (Card Diamonds (Other 7)), (Card Hearts (Other 10)), (Card Diamonds (Other 10)), (Card Spades (Other 10))])) == True)
    testisStraightFlush = T.TestCase $ T.assertBool "testisStraightFlush" ((isStraightFlush (Hand [(Card Diamonds (Other 4)), (Card Diamonds (Other 7)), (Card Diamonds (Other 6)), (Card Diamonds (Other 3)), (Card Diamonds (Other 5))])) == True)
    testisRoyalStraightFlush1 = T.TestCase $ T.assertBool "testisRoyalStraightFlush1" ((isRoyalStraightFlush (Hand [(Card Diamonds J), (Card Hearts K), (Card Diamonds A), (Card Diamonds Q), (Card Diamonds (Other 10))])) == False)
    testisRoyalStraightFlush2 = T.TestCase $ T.assertBool "testisRoyalStraightFlush2" ((isRoyalStraightFlush (Hand [(Card Diamonds K), (Card Diamonds Q), (Card Diamonds A), (Card Diamonds J), (Card Diamonds (Other 10))])) == True)
    testhandValues = T.TestCase $ T.assertBool "testhandValues" ((handValues (Hand [(Card Diamonds (Other 5)), (Card Diamonds J), (Card Diamonds A), (Card Diamonds (Other 5)), (Card Diamonds (Other 10))])) == [5,5,10,11,14])

    testListP5 = T.TestList [testisPair,
                      testisPair2,
                      testisThreeOfAKind,
                      testisStraight,
                      testisStraight2,
                      testisRoyalStraightFlush1,
                      testisRoyalStraightFlush2,
                      testisStraightFlush,
                      testisFourOfAKind,
                      testisFullHouse,
                      testisFlush1,
                      testisFlush2,
                      testhandValues]
