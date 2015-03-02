module Player where

  import Deck
  import Hand

  data PlayerRole = Dealer
                  | Shark

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

  {-
    TODO
    PURPOSE: return state of player
  -}
  stateForPlayer :: GamePlayer -> PlayerState
  stateForPlayer (Player _ _ (State state)) = undefined

  {-
    TODO
    PURPOSE: return the hand of a player.
  -}
  handForPlayer :: GamePlayer -> PlayingHand
  handForPlayer (Player hand _ _) = hand

  {-
    TODO
    PURPOSE: change the state of a player to the supplied state.
  -}
  editStateForPlayer :: GamePlayer -> PlayerState -> GamePlayer
  editStateForPlayer = undefined

  {-
    TODO
    PURPOSE: check if a player is a dealer or not
    HINT: Pattern-match the Constructor of the datatype PlayerRole.
  -}
  isDealer :: GamePlayer -> Bool
  isDealer (Player _ role _) = undefined

  {-
    PURPOSE: create a player, aka shark.
  -}
  createShark :: GamePlayer
  createShark = (Player EmptyHand Shark UndefinedState)

  {-
    PURPOSE: create a dealer
  -}
  createDealer :: GamePlayer
  createDealer = (Player EmptyHand Dealer UndefinedState)
