module Games.BlackJack where

  import qualified Test.HUnit as T
  import Game
  import Card
  import Hand
  import Player
  import Deck

  data GameState = GState [GamePlayer] PlayingDeck

  instance GameValue PlayingCard where
    valueOf (Card _ (Other value)) = value
    valueOf (Card _ A) = 11
    valueOf (Card _ _) = 10

  instance GameValue PlayingHand where
    valueOf (Hand []) = 0
    valueOf (Hand (card:rest)) = (valueOf card) + (valueOf (Hand rest))

  instance GameValue PlayingDeck where
    valueOf (Deck []) = 0
    valueOf (Deck (card:rest)) = (valueOf card) + (valueOf (Deck rest))

  instance GameEq PlayingCard where
    (===) (Card _ K) (Card _ K) = True
    (===) (Card _ Q) (Card _ Q) = True
    (===) (Card _ J) (Card _ J) = True
    (===) (Card _ A) (Card _ A) = True
    (===) (Card _ (Other valueA)) (Card _ (Other valueB)) = valueA == valueB
    (===) _ _ = False

  instance Show GameState where
    show (GState [] deck) = "deck consists of " ++ show(deck)
    show (GState (dealer@(Player _ Dealer _):rest) deck) = "Dealer: " ++ show(dealer) ++ ", " ++ show(GState rest deck)
    show (GState (player:rest) deck) = "Player: " ++ show(player) ++ ", " ++ show(GState rest deck)

  {-
    PURPOSE: main function to fire it all up.
  -}
  main :: IO ()
  main = do
    putStrLn ("Welcome to " ++ show(BJ))
    gameState <- playerPhase
    putStrLn (show(gameState))

  {-
    TODO
    PURPOSE: iterate every player in a supplied gamestate and a apply a certain function that
      takes a player as an argument and returns a player, then return the gamestate.
  -}
  mapPlayers :: (GamePlayer -> GamePlayer) -> GameState -> GameState
  mapPlayers f gameState = undefined

  {-
    TODO
    PURPOSE: infinite loop until game is done, loop through players and ask for actions,
      deal cards, etc etc.
  -}
  gamePhase :: GameState -> IO ()
  gamePhase gameState = do undefined

  {-
    TODO
    PURPOSE: check if hand is 21 or not
  -}
  isTwentyOne :: PlayingHand -> Bool
  isTwentyOne hand = undefined

  {-
    TODO
    PURPOSE: check if hand is fat (above 21) or not
  -}
  isFat :: PlayingHand -> Bool
  isFat hand = undefined

  {-
    TODO
    PURPOSE: check if hand is 17 or above, for the dealer.
  -}
  isAbove17 :: PlayingHand -> Bool
  isAbove17 hand = undefined

  {-
    TODO
    PURPOSE: check if the hand has Black Jack or not
    HOW: check if there's two cards in hand and that it is 21.
  -}
  hasBlackJack :: PlayingHand -> Bool
  hasBlackJack hand = undefined

  {-
    valueOfPlayerHand player
    PURPOSE: calculate value of hand, with Ace having two values, 1 or 11.
  -}
  valueOfPlayerHand :: PlayingHand -> Int
  valueOfPlayerHand hand =
    let
      valueOfPlayerHand' :: PlayingHand -> Int -> Int
      valueOfPlayerHand' hand value
        | (value == 0) = valueOfPlayerHand' hand (valueOf hand) --put the value of the hand inside of the val arg and recurse.
        | (value > 21) =
            let
              numberOfAces = numberOfValuesInHand hand A
              difference = value - 21
              minimumAcesNeeded = ceiling ((toRational difference) / 10.0)
            in
              if (numberOfAces > 0) then
                if (numberOfAces >= minimumAcesNeeded) then
                  valueOfPlayerHand' hand (value - 10 * minimumAcesNeeded) --remove the number of aces needed.
                else
                  (value - 10 * numberOfAces) --return with minimum value, still fat hand.
              else
                value
        | (value > 0) = value
    in
      valueOfPlayerHand' hand 0

  {-
    PURPOSE: count every card in hand that contains value of the supplied card (dont give a shit about the suit)
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
    in
      numberOfValuesInHand' hand needle 0

  {-
    PURPOSE: the phase where the user defines the number of players and generates
      a matching gamestate for it.
  -}
  playerPhase :: IO GameState
  playerPhase = do
    putStr ("How many players are participating? [1 - 6]: ")
    userInput <- getLine
    let numberOfPlayers = read userInput :: Int
    return (generateGameStateForPlayers numberOfPlayers)

  {-
    PURPOSE: generate a gamestate for n players and a dealer.
  -}
  generateGameStateForPlayers :: Int -> GameState
  generateGameStateForPlayers number =
    let
      generateGameStateForPlayers' :: Int -> [GamePlayer]
      generateGameStateForPlayers' 0 = []
      generateGameStateForPlayers' number = createShark : (generateGameStateForPlayers' (number - 1))
    in
      (GState (createDealer : (generateGameStateForPlayers' number)) createEmptyDeck)

  {-
    PURPOSE: print a players hand
  -}
  printHand :: PlayingHand -> IO ()
  printHand hand = do
    putStrLn $ "Your hand: " ++ (show hand) ++ "."
    putStrLn $ "Hand value " ++ (show (valueOf hand::Int)) ++ "."

  {-
    PURPOSE: list every possible state a blackjack player could have
  -}
  states :: [PlayerState]
  states = [(State "HIT"), (State "UNKNOWN"), (State "SPLIT"), (State "STAND"), (State "DOUBLE")]

  {-
    PURPOSE: deal a card to one player from a provided deck
  -}
  dealCard :: PlayingDeck -> GamePlayer -> IO GamePlayer
  dealCard EmptyDeck player = return player
  dealCard deck (Player hand role state) = return (Player (addCardToHand hand (drawCardFromDeck deck)) role state)

  {-
    PURPOSE: create a blackjack deck, consists of one deck.
  -}
  createDeck :: IO PlayingDeck
  createDeck = return (shuffleDeck (createEmptyDeck))

  {-
    PURPOSE: return every playable
  -}
  statesAvailable :: PlayingHand -> [PlayerState]
  statesAvailable (Hand (cards))
    | length (cards) == 2 && head cards /== last cards = [(State "DOUBLE"), (State "HIT"),(State "STAND")]
    | length (cards) == 2 && valueOf (head cards) == valueOf(last (cards)) = [(State "SPLIT"), (State "DOUBLE"), (State "HIT"),(State "STAND")]
    | length (cards) > 2 && 9 <= valueOf (Hand cards) && valueOf (Hand cards) <= 11 = [(State "DOUBLE"), (State "HIT"),(State "STAND")]
    | otherwise = [(State "HIT"),(State "STAND")]

  {-
    PURPOSE: perform a move for one player.
  -}

  performMove :: GamePlayer -> PlayingDeck -> [GamePlayer]
  performMove (Player (Hand (card:cards)) roles (State "SPLIT")) deck = [(Player (Hand [card]) roles (State "SPLIT")),(Player (Hand cards) roles (State "SPLIT"))]
  performMove (Player hand role (State "HIT")) deck = [(Player (addCardToHand hand (drawCardFromDeck deck)) role (UndefinedState))]
  performMove (Player hand role (State "DOUBLE")) deck = [(Player (addCardToHand hand (drawCardFromDeck deck)) role (State "DOUBLE"))]
  performMove (Player hand role (State "STAND")) deck = [(Player hand role (UndefinedState))]
  performMove _ deck = undefined

  {-
    TODO: Test cases
  -}
  testValuesInHand = T.TestCase $ T.assertBool "testValuesInHand" (numberOfValuesInHand (Hand [(Card Diamonds A), (Card Spades A), (Card Clubs (Other 5))]) A == 2)
  testValuesInHand2 = T.TestCase $ T.assertBool "testValuesInHand2" (numberOfValuesInHand (Hand [(Card Diamonds A), (Card Spades Q), (Card Clubs (Other 5))]) K == 0)
  testBJHasBlackJack = T.TestCase $ T.assertBool "testBJHasBlackJack" (hasBlackJack (Hand [(Card Diamonds A), (Card Spades K)]) == True)
  testBJHasBlackJack2 = T.TestCase $ T.assertBool "testBJHasBlackJack2" (hasBlackJack (Hand [(Card Diamonds A), (Card Spades (Other 2))]) == False)
  testBJHasBlackJack3 = T.TestCase $ T.assertBool "testBJHasBlackJack3" (hasBlackJack (Hand [(Card Diamonds (Other 7)), (Card Clubs (Other 7)), (Card Spades (Other 7))]) == False)
  testBJCalculateFatHand = T.TestCase $ T.assertBool "testFatHand" (valueOfPlayerHand (Hand [(Card Diamonds K), (Card Clubs Q), (Card Spades (Other 3))]) == 23)
  testBJFuckedUpHand = T.TestCase $ T.assertBool "testFuckedUpHand" (valueOfPlayerHand (Hand [(Card Diamonds A), (Card Clubs A), (Card Hearts A), (Card Spades A), (Card Spades (Other 7))]) == 21)
  testBJCalculateAceHand = T.TestCase $ T.assertBool "testAceHand" (valueOfPlayerHand (Hand [(Card Diamonds A), (Card Clubs A)]) == 12)
  testBJCalculateAce21 = T.TestCase $ T.assertBool "testAce21" (valueOfPlayerHand (Hand [(Card Diamonds A), (Card Clubs A), (Card Hearts (Other 9))]) == 21)
  testBJDrawCardFromDeck = T.TestCase $ T.assertBool "testBJDrawCardFromDeck" ((createEmptyDeck) == testDeck)
  testBJstatesAvailable = T.TestCase $ T.assertBool "testBJstatesAvailable" (statesAvailable (Hand [(Card Diamonds (Other 3)), (Card Clubs (Other 5))]) == ([(State "DOUBLE"), (State "HIT"),(State "STAND")]))
  testBJstatesAvailable2 = T.TestCase $ T.assertBool "testBJstatesAvailable2" (statesAvailable (Hand [(Card Diamonds K), (Card Clubs K)]) == ([(State "SPLIT"), (State "DOUBLE"), (State "HIT"),(State "STAND")]))
  testBJstatesAvailable3 = T.TestCase $ T.assertBool "testBJstatesAvailable3" (statesAvailable (Hand [(Card Diamonds (Other 4)), (Card Clubs (Other 3)), (Card Hearts (Other 3))]) == ([(State "DOUBLE"), (State "HIT"),(State "STAND")]))
  testBJstatesAvailable4 = T.TestCase $ T.assertBool "testBJstatesAvailable4" (statesAvailable (Hand [(Card Diamonds (Other 7)), (Card Clubs (Other 3)), (Card Hearts (Other 3))]) == ([(State "HIT"),(State "STAND")]))

  testListBJ = T.TestList [testCreateEmptyDeck,
                          testDrawCardFromDeck,
                          testBJstatesAvailable,
                          testBJstatesAvailable2,
                          testBJstatesAvailable3,
                          testBJstatesAvailable4,
                          testBJCalculateFatHand,
                          testBJCalculateAceHand,
                          testBJCalculateAce21,
                          testBJFuckedUpHand,
                          testBJHasBlackJack,
                          testBJHasBlackJack2,
                          testBJHasBlackJack3,
                          testValuesInHand,
                          testValuesInHand2]
