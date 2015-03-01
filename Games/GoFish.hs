module Games.GoFish where

  import Game
  import Card
  import Hand
  import Player

  data GameState = GState [GamePlayer]

  instance Show GameState where
    show (GState []) = "No players GF"
