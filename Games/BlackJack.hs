module Games.BlackJack where

  import qualified Test.HUnit as T
  import Control.Monad
  import Data.Char
  import Interface
  import Game
  import Card
  import Hand
  import Player
  import Deck

  type PlayersAndDeck = ([GamePlayer], PlayingDeck)

  instance GameValue PlayingCard where
    valueOf (Card _ (Other value)) = value
    valueOf (Card _ A) = 11
    valueOf (Card _ _) = 10
    valueOf _ = 0

  instance GameValue PlayingHand where
    valueOf (Hand []) = 0
    valueOf (Hand (card:rest)) = (valueOf card) + (valueOf (Hand rest))
    valueOf _ = 0

  instance GameValue PlayingDeck where
    valueOf (Deck []) = 0
    valueOf (Deck (card:rest)) = (valueOf card) + (valueOf (Deck rest))
    valueOf _ = 0

  instance GameEq PlayingCard where
    (===) (Card _ K) (Card _ K) = True
    (===) (Card _ Q) (Card _ Q) = True
    (===) (Card _ J) (Card _ J) = True
    (===) (Card _ A) (Card _ A) = True
    (===) (Card _ (Other valueA)) (Card _ (Other valueB)) = valueA == valueB
    (===) _ _ = False

  {-
    PURPOSE: main function to fire it all up.
  -}
  main :: IO ()
  main = do
    putStrLn ("Welcome to " ++ show(BJ))
    setupGameState <- setupPhase
    gamePhase setupGameState
    return ()

  {-
    TODO
    PURPOSE: infinite loop until game is done, loop through players and ask for actions,
      deal cards, etc etc.
  -}
  gamePhase :: GameState -> IO GameState
  gamePhase gameState@(GState _ deck) =
    do
    printGameState gameState
    (GState dPlayers dDeck) <- dealerPhase gameState
    (GState pPlayers pDeck) <- playerPhase gameState
    -- players <- (forM (playersInGameState dealerGameState) (\player -> do
    --   (playerPhase player deck)
    --   ))
    -- let (_, newDeck) = last players
    -- let newPlayers = concat $ map (\(newPlayers, _) -> newPlayers) players
    -- let dealers = dealersInGameState dealerGameState
    -- let gamestate = (GState (newPlayers ++ dealers) newDeck)
    do
    gamePhase gameState

  {-
    PURPOSE: read move of player
  -}
  readMove :: GamePlayer -> IO PlayerState
  readMove player =
    let
      states = statesAvailable $ handForPlayer player
    in do
      putStr ("Make your move " ++ (show states) ++ ": ")
      input <- getLine
      let stateIdentifier = map toUpper input
      let state = (State stateIdentifier)
      do
        if elem state states then do
          return state
        else do
          readMove player

  {-
    PURPOSE: wait for user input.
  -}
  playerPhase :: GameState -> IO GameState
  playerPhase gameState@(GState _ deck) =
    let

      dealers = playersWithRoleInGameState gameState Dealer

      --([GamePlayer], PlayingDeck)
      playerPhase' :: GamePlayer -> PlayersAndDeck
      playerPhase' player = 
        let
          states = statesAvailable $ handForPlayer player --should provide player instead of hand. TODO
        in
          undefined


      playerPhase'' :: [PlayersAndDeck] -> GameState
      playerPhase'' _ = undefined


        -- let
        --   states = statesAvailable $ handForPlayer player
        -- in do
        --   state <- (readMove player deck)
        --   let newPlayer = (editStateForPlayer player state)
        --   let (performedPlayer, newDeck) = performMove newPlayer deck
        --   return (performedPlayer, newDeck)
    in do
      --return (GState (playerPhase' (playersWithRoleInGameState gameState Shark) deck) deck)
      undefined


  {-
    TODO
    PURPOSE: dealer draws card and adds it to hand if less than 17
  -}
  dealerPhase :: GameState -> IO GameState
  dealerPhase gameState
    | True = return gameState
    | otherwise = undefined

  {-
    PURPOSE:
  -}

  {-
    PURPOSE: the phase where the user defines the number of players and generates
      a matching gamestate for it.
  -}
  setupPhase :: IO GameState
  setupPhase = do
    putStr ("How many players are participating? [1 - 7]: ")
    userInput <- getLine
    let numberOfPlayers = read userInput :: Int
    return (generateGameStateForPlayers numberOfPlayers)

  {-
    TODO
    PURPOSE: print gameState as a userInterface.
    SIDE-EFFECTS: output to the terminal.
  -}
  printGameState :: GameState -> IO ()
  printGameState gameState = do
    printDivider
    printDealers gameState
    printPlayers gameState
    printDivider

  {-
    PURPOSE: print player cards in a nice way, not the dealers card.
    SIDE-EFFECTS: output to the terminal
  -}
  printPlayers :: GameState -> IO ()
  printPlayers gameState =
    let
      printPlayers' :: [GamePlayer] -> String
      printPlayers' [] = []
      printPlayers' ((Player hand _ _):[]) = show hand ++ (printPlayers' [])
      printPlayers' ((Player hand _ _):rest) = show hand ++ " | " ++ (printPlayers' rest)
    in
      printLnCenter $ printPlayers' (playersWithRoleInGameState gameState Shark)

  {-
    PURPOSE: print dealer cards in a nice way
    SIDE-EFFECTS: output to the terminal
  -}
  printDealers :: GameState -> IO ()
  printDealers gameState =
    let
      printDealers' :: [GamePlayer] -> String
      printDealers' _ = undefined
  -- putStrLn $ repeatCharacter '\n' 5
  -- printDealers ((Player hand Dealer _):rest) = do
  --   printLnCenter $ (show hand)
  --   printDealers rest
  -- printDealers (_:rest) = printDealers rest
    in
      undefined


  {-
    isTwentyOne hand
    PURPOSE: check if hand is 21 or not
    PRE: True
    POST: Returns a bool if the player have 21 or not
    SIDE EFFECTS: None
    EXAMPLES:
        isTwentyOne (Hand [(Card Diamonds A), (Card Clubs A), (Card Hearts (Other 9))]) = True
        isTwentyOne (Hand [(Card Diamonds A), (Card Clubs A), (Card Hearts (Other 8))]) = False
  -}
  isTwentyOne :: PlayingHand -> Bool
  isTwentyOne hand = valueOfPlayerHand hand == 21

  {-
    isFat hand
    PURPOSE: check if hand is fat (above 21) or not
    PRE: True
    POST: Returns a bool of hand if the player is "fat" (hand value over 21) or not.
    SIDE EFFECTS: None
    EXAMPLES:
        isFat (Hand [(Card Diamonds A), (Card Clubs A), (Card Hearts (Other K))]) = True
        isFat (Hand [(Card Diamonds A), (Card Clubs A), (Card Hearts (Other 4))]) = False
  -}
  isFat :: PlayingHand -> Bool
  isFat hand = valueOfPlayerHand hand > 21

  {-
    isAbove16 hand
    PURPOSE: check if hand is 17 or above, for the dealer.
    PRE: True
    POST: Returns a bool of hand if the value of the players hand is eq or above 17.
    SIDE EFFECTS: None
    EXAMPLES:
        isAbove16 (Hand [(Card Diamonds A), (Card Clubs A), (Card Hearts (Other 5))]) = True
        isAbove16 (Hand [(Card Diamonds A), (Card Clubs A), (Card Hearts (Other 2))]) = False
  -}
  isAbove16 :: PlayingHand -> Bool
  isAbove16 hand = valueOfPlayerHand hand >= 17

  {-
    hasBlackJack hand
    PURPOSE: check if the hand has Black Jack or not
    PRE: True
    POST: Returns a bool of hand, if the value of the players hand is 21 has 2 cards in hand or not.
    SIDE EFFECTS: None
    EXAMPLES:
        isAbove16 (Hand [(Card Diamonds A), (Card Clubs k)]) = True
        isAbove16 (Hand [(Card Diamonds A), (Card Clubs 5)]) = False
        isAbove16 (Hand [(Card Diamonds A), (Card Clubs 5), (Card Hearts (Other 5))]) = False
  -}
  hasBlackJack :: PlayingHand -> Bool
  hasBlackJack hand = cardsInHand hand == 2 && valueOfPlayerHand hand == 21

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
  statesAvailable hand@(Hand cards)
    | (cardsInHand hand == 2 && head cards /== last cards) = [(State "DOUBLE"), (State "HIT"), (State "STAND")]
    | (cardsInHand hand == 2 && valueOf (head cards) == valueOf(last (cards))) = [(State "SPLIT"), (State "DOUBLE"), (State "HIT"), (State "STAND")]
    | ((cardsInHand hand) > 2 && (valueOfPlayerHand hand) >= 9 && (valueOfPlayerHand hand) <= 11) = [(State "DOUBLE"), (State "HIT"), (State "STAND")]
    | otherwise = [(State "HIT"), (State "STAND")]
  statesAvailable EmptyHand = [(State "HIT"), (State "STAND")]

  {-
    PURPOSE: perform a move for one player.
  -}
  performMove :: GamePlayer -> PlayingDeck -> ([GamePlayer], PlayingDeck)
  performMove (Player (Hand (card:cards)) roles (State "SPLIT")) deck = ([(Player (Hand [card]) roles (State "SPLIT")), (Player (Hand cards) roles (State "SPLIT"))], deck)
  performMove (Player hand role (State "HIT")) deck =
    let
      (card, newDeck) = drawAndRemoveCardFromDeck deck
    in
      ([(Player (addCardToHand hand card) role (UndefinedState))], newDeck)
  performMove (Player hand role (State "DOUBLE")) deck =
    let
      (card, newDeck) = drawAndRemoveCardFromDeck deck
    in
      ([(Player (addCardToHand hand card) role (State "DOUBLE"))], newDeck)

  performMove (Player hand role (State "STAND")) deck = ([(Player hand role (UndefinedState))], deck)
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
  -- testBJperformMoveSplit = T.TestCase $ T.assertBool "testBJperformMoveSplit" (((performMove (Player (Hand [(Card Clubs K),(Card Diamonds K)]) Shark (State "SPLIT")) testDeck)) == [(Player (Hand [(Card Clubs K)]) Shark (State "SPLIT")), (Player (Hand [(Card Diamonds K)]) Shark (State "SPLIT"))])
  -- testBJperformMoveHit = T.TestCase $ T.assertBool "testBJperformMoveHit" ((performMove (Player testHand Shark (State "HIT")) testDeck) == [(Player (Hand [(Card Spades A), (Card Diamonds A),(Card Spades (Other 5)),(Card Clubs K), (Card Diamonds (Other 2))]) Shark UndefinedState)])
  -- testBJperformMoveDouble = T.TestCase $ T.assertBool "testBJperformMoveDouble" ((performMove (Player testHand Shark (State "DOUBLE")) testDeck) == [(Player (Hand [(Card Spades A), (Card Diamonds A),(Card Spades (Other 5)),(Card Clubs K), (Card Diamonds (Other 2))]) Shark (State "DOUBLE"))])
  -- testBJperformMoveStand = T.TestCase $ T.assertBool "testBJperformMoveStand" ((performMove (Player (Hand [(Card Clubs K),(Card Diamonds K)]) Shark (State "STAND")) testDeck) == [(Player (Hand [(Card Clubs K),(Card Diamonds K)]) Shark (UndefinedState))])

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
                          testValuesInHand2
                          -- testBJperformMoveSplit,
                          -- testBJperformMoveHit,
                          -- testBJperformMoveDouble,
                          -- testBJperformMoveStand
                          ]
