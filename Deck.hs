module Deck where

  import Card
  import Game

  {- DATA -}

  data PlayingDeck = Deck [PlayingCard]
                   | EmptyDeck

  {- INSTANCES -}

  {-
    PURPOSE: convert every card in a deck into a string and show i nicely.
  -}
  instance Show PlayingDeck where
    show (Deck []) = ""
    show (Deck (card:rest)) = show card ++ show (Deck rest)

  {- FUNCTIONS -}

  {-
    PURPOSE: Create a playingCard deck with 52 cards, unshuffled.
  -}
  createEmptyDeck :: Game -> PlayingDeck
  createEmptyDeck game = (Deck (concat $ map (\suit -> (map (\value -> (Card suit value game)) [A .. K])) [Spades ..]))

  {-
    TODO
    PURPOSE: Shuffle the supplied deck
  -}
  shuffleDeck :: PlayingDeck -> PlayingDeck
  shuffleDeck deck = undefined

  {-
    PURPOSE: Draw one card from the top of the deck, if there's no more cards
      return InvisibleCard
  -}
  drawCardFromDeck :: PlayingDeck -> PlayingCard
  drawCardFromDeck EmptyDeck = InvisibleCard
  drawCardFromDeck (Deck (card:_)) = card
