module Deck where

  import Card
  import Game

  {- DATA -}

  data PlayingDeck = Deck [PlayingCard]
                   | Empty

  {- FUNCTIONS -}

  {-
    PURPOSE: Create a playingCard deck with 52 cards, unshuffled.
  -}
  createEmptyDeck :: PlayingDeck
  createEmptyDeck = undefined

  {-
    PURPOSE: Shuffle the supplied deck
  -}
  shuffleDeck :: PlayingDeck -> PlayingDeck
  shuffleDeck deck = undefined

  {-
    PURPOSE: Draw one card from the top of the deck, if there's no more cards
      return InvisibleCard
    HINT: head
  -}
  drawCardFromDeck :: PlayingDeck -> PlayingCard
  drawCardFromDeck Empty = undefined
  drawCardFromDeck deck = undefined
