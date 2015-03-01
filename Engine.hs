module Engine where

  import Game
  import Deck

  {-
    PURPOSE: provide the game in question the function to create a deck.
  -}
  provideDeck :: PlayingDeck
  provideDeck = createEmptyDeck

  {-
    TODO
    - think of generalized actions and operations in a ordinary cardGame.
    - implement them and return the function requested (not a value, look
      at provideDeck).
      + game name?
      + deck
      + dealer?
  -}
