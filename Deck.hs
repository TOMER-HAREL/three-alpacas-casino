module Deck where

  import Card
  import Game

  {- DATA -}

  data PlayingDeck = Deck [PlayingCard]
                   | Empty

  {- INSTANCES -}

  {-
    TODO
    PURPOSE: convert every card in a deck into a string and show i nicely.
    HINT: PlayingCard already has functionality for show(PlayingCard), just
      implement a loop to iterate through all of the cards.
  -}
  instance Show PlayingDeck where
    show (Deck (card:rest)) = undefined

  {- FUNCTIONS -}

  {-
    TODO
    PURPOSE: Create a playingCard deck with 52 cards, unshuffled.
  -}
  createEmptyDeck :: PlayingDeck
  createEmptyDeck = undefined

  {-
    TODO
    PURPOSE: Shuffle the supplied deck
  -}
  shuffleDeck :: PlayingDeck -> PlayingDeck
  shuffleDeck deck = undefined

  {-
    TODO
    PURPOSE: Draw one card from the top of the deck, if there's no more cards
      return InvisibleCard
    HINT: head
  -}
  drawCardFromDeck :: PlayingDeck -> PlayingCard
  drawCardFromDeck Empty = undefined
  drawCardFromDeck deck = undefined
