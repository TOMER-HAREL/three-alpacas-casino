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


  createShark :: GamePlayer
  createShark = (Player EmptyHand Shark UndefinedState)

  createDealer :: GamePlayer
  createDealer = (Player EmptyHand Dealer UndefinedState)
