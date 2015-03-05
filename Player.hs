module Player where

  import Card
  import Deck
  import Hand
  import qualified Test.HUnit as T

  data PlayerRole = Dealer
                  | Shark deriving (Eq)

  data PlayerState = State String
                   | UndefinedState

  data GamePlayer = Player PlayingHand PlayerRole PlayerState

  instance Show PlayerRole where
    show Dealer = "Dealer"
    show Shark = "Shark"

  instance Show PlayerState where
    show (State state) = state
    show UndefinedState = "Undefined Status"

  instance Show GamePlayer where
    show (Player hand role state) = "[" ++ show(role) ++ ", " ++ show(state) ++ "] " ++ show(hand)

  instance Eq PlayerState where
    (==) (State stateA) (State stateB) = stateA == stateB
    (==) UndefinedState UndefinedState = True
    (==) _ _ = False

  instance Eq GamePlayer where
    (==) (Player handa rolea statea) (Player handb roleb stateb) = handa == handb && rolea == roleb && statea == stateb

  {-
    stateForPlayer player
    PURPOSE: return state of player
    PRE:  true
    POST: the state of the player
    SIDE EFFECTS: none
    EXAMPLES: stateForPlayer (Player (Hand [(Card Diamonds K), (Card Clubs K)]) Shark (State "HIT")) = "HIT"
  -}
  stateForPlayer :: GamePlayer -> PlayerState
  stateForPlayer (Player _ _ UndefinedState) = UndefinedState
  stateForPlayer (Player _ _ (State state)) = (State state)

  {-
    handForPlayer player
    PURPOSE: return hand of the player
    PRE:  true
    POST: the hand of the player
    SIDE EFFECTS: none
    EXAMPLES: handForPlayer (Player (Hand [(Card Diamonds K), (Card Clubs K)]) Shark (State "STAND")) = [KD] [KC]
  -}
  handForPlayer :: GamePlayer -> PlayingHand
  handForPlayer (Player hand _ _) = hand

  {-
    editStateForPlayer player state
    PURPOSE: change the state of a player to the supplied state.
    PRE:  true
    POST: the player with the given state
    SIDE EFFECTS: none
    EXAMPLES: editStateForPlayer (Player (Hand [(Card Diamonds K), (Card Clubs K)]) Shark (State "HIT")) (State "STAND") = [Shark, STAND] [KD] [KC]
  -}
  editStateForPlayer :: GamePlayer -> PlayerState -> GamePlayer
  editStateForPlayer (Player hand role UndefinedState) state  = (Player hand role state)
  editStateForPlayer (Player hand role _) state = (Player hand role state)

  {-
    isDealer player
    PURPOSE: check if a player is a dealer or not
    PRE:  true
    POST: bool that describes if the player are a dealer or not
    SIDE EFFECTS: none
    EXAMPLES: isDealer (Player (Hand [(Card Diamonds K), (Card Clubs K)]) Dealer (State "HIT")) = True
  -}
  isDealer :: GamePlayer -> Bool
  isDealer (Player _ Dealer _) = True
  isDealer (Player _ _ _) = False


  {-
    createShark
    PURPOSE: create a player, aka shark.
    PRE:  true
    POST: a shark player
    SIDE EFFECTS: none
    EXAMPLES: createShark = [Shark, Undefined Status] Empty Hand
  -}
  createShark :: GamePlayer
  createShark = (Player EmptyHand Shark UndefinedState)

  {-
    createDealer
    PURPOSE: create a dealer
    PRE:  true
    POST: a dealer
    SIDE EFFECTS: none
    EXAMPLES: createDealer = [Dealer, Undefined Status] Empty Hand
  -}
  createDealer :: GamePlayer
  createDealer = (Player EmptyHand Dealer UndefinedState)

  {- TESTS -}
  teststateForPlayerShark = T.TestCase $ T.assertBool "stateForPlayerShark" (stateForPlayer (Player testHand Shark (State "HIT")) == (State "HIT"))
  teststateForPlayerDealer = T.TestCase $ T.assertBool "stateForPlayerDealer" (stateForPlayer (Player testHand Dealer (State "STAND")) == (State "STAND"))
  testhandForPlayer = T.TestCase $ T.assertBool "handForPlayer" (handForPlayer (Player testHand Dealer (State "STAND")) == (Hand [(Card Diamonds A),(Card Spades (Other 5)),(Card Clubs K),(Card Diamonds (Other 2))]))
  testeditStateForPlayer = T.TestCase $ T.assertBool "editStateForPlayer" (editStateForPlayer (Player testHand Shark (State "HIT")) (State "STAND") == (Player testHand Shark (State "STAND")))
  testisDealer1 =  T.TestCase $ T.assertBool "isDealer1" (isDealer (Player testHand Dealer (State "HIT")) == True)
  testisDealer2 =  T.TestCase $ T.assertBool "isDealer2" (isDealer (Player testHand Shark (State "HIT")) == False)
  testcreateShark =  T.TestCase $ T.assertBool "createShark" (createShark == (Player EmptyHand Shark UndefinedState))
  testcreateDealer = T.TestCase $ T.assertBool "createDealer" (createDealer == (Player EmptyHand Dealer UndefinedState))

  testlistPlayer = T.TestList [teststateForPlayerShark,
                                teststateForPlayerDealer,
                                testhandForPlayer,
                                testeditStateForPlayer,
                                testisDealer1,
                                testisDealer2,
                                testcreateShark,
                                testcreateDealer]
