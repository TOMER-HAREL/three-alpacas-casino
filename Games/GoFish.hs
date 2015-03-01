module Games.GoFish where

  import Game
  import Card
  import Hand
  import Player
  import Deck

  data GameState = GState [GamePlayer]

  instance Show GameState where
    show (GState []) = "No players GF"


  createDeck :: PlayingDeck
  createDeck = shuffleDeck (createEmptyDeck)

  dealCards :: PlayingDeck -> GamePlayer -> GamePlayer
  dealCards EmptyDeck _ = undefined
  dealCards deck player = undefined
