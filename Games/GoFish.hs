module Games.GoFish where

  import Game
  import Card
  import Hand
  import Deck
  import Player


  createDeck :: PlayingDeck
  createDeck = shuffleDeck (createEmptyDeck GF)

  dealCards :: PlayingDeck -> GamePlayer -> GamePlayer
  dealCards deck player = undefined
  dealCards EmptyDeck _ = undefined
