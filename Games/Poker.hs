module Games.Poker where

    import qualified Test.HUnit as T
    import Game
    import Card
    import Hand
    import Player
    import Deck

    data GameState = GState [GamePlayer] PlayingDeck

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

    instance Show GameState where
          show (GState [] deck) = "deck consists of " ++ show(deck)
          show (GState (player:rest) deck) = "Player: " ++ show(player) ++ ", " ++ show(GState rest deck)

    main :: IO ()
    main = do
        putStrLn "vääääälkommen till oss"


    --If isStraightFlush is True map the list and look if fst is an A
    isRoyalStraigtFlush:: PlayingHand -> Bool
    isRoyalStraigtFlush hand = undefined

    --If we got an isStraight and isFlush at the same hand isStraightFlush will return true
    isStraightFlush :: PlayingHand -> Bool
    isStraightFlush hand = undefined

    --Look up if we got 4 card of the same value
    isFourOfAKind :: PlayingHand -> Bool
    isFourOfAKind hand = undefined

    --if we got one isPair and isThreeOfAKind at the same hand the full house will return True
    isFullHouse :: PlayingHand -> Bool
    isFullHouse hand = undefined

    --if all cards in one hand has the suit isFlush will return True
    isFlush :: PlayingHand -> Bool
    isFlush hand = undefined

    -- All cards on hand in numerical order
    isStraight :: PlayingHand -> Bool
    isStraight hand = undefined

    --Three cards of the same value
    isThreeOfAKind :: PlayingHand -> Bool
    isThreeOfAKind hand = let
                    numbers = map (\value -> numberOfValuesInHand hand value) [A .. K]
                    in
                      elem 3 numbers

    --if we got two isPair in one hand, isTwoPair will return True
    isTwoPair :: PlayingHand -> Bool
    isTwoPair hand = undefined

    --if we got two cards of same value, isPair will returnn True
    isPair :: PlayingHand -> Bool
    isPair hand = let
                    numbers = map (\value -> numberOfValuesInHand hand value) [A .. K]
                    in
                      elem 2 numbers


    {- TESTS -}

    testisPair = T.TestCase $ T.assertBool "testisPair" ((isPair (Hand [(Card Diamonds (Other 5)), (Card Hearts (Other 5)), (Card Clubs (Other 3)), (Card Spades K), (Card Diamonds (Other 7))])) == True)

    testListP5 = T.TestList [testisPair]
