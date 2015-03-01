module Player where

  import Deck
  import Game
  import Hand


  data PlayerRole = Dealer
                  | Shark

  data PlayerState = State String
                   | UndefinedState

  data GamePlayer = Player PlayingHand PlayerRole PlayerState Game

  instance Show GamePlayer where
    show (Player hand role (State state) game) = state ++ " : " ++ show(hand)

  createShark :: Game -> GamePlayer
  createShark game = (Player (emptyHand game) Shark UndefinedState game)

  createDealer :: Game -> GamePlayer
  createDealer game = (Player (emptyHand game) Dealer UndefinedState game)
