module Player where

  import Deck
  import Game
  import Hand


  data PlayerRole = Dealer
                  | Shark

  data PlayerState = State String
                   | UndefinedState

  data GamePlayer = Player PlayingHand PlayerRole PlayerState

  instance Show GamePlayer where
    show (Player hand role (State state)) = state ++ " : " ++ show(hand)


  createShark :: GamePlayer
  createShark = (Player (emptyHand) Shark UndefinedState)

  createDealer :: GamePlayer
  createDealer = (Player (emptyHand) Dealer UndefinedState)
