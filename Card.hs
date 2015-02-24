{-use this as a base class for BJCard, PokerCard etc. -}
module Card (PlayingCard(..)) where

  import Game

  {-
    PURPOSE: to remove the hardcoded suitss of a card as we're designing for
             multiple card games. Creating a class let's us design our own
             functions for a certain card.
  -}
  class Card a where
    suits :: a -> Int

  {-
    PURPOSE: Make the card more generic, that would be different suitss for
      different games.
    EXAMPLES:
      suits (Card Diamonds A None) == 13
      suits (Card Diamonds A BJ) == 11
  -}
  instance Card PlayingCard where
    -- Black Jack
    suits (Card _ (Other suits) BJ) = suits
    suits (Card _ A BJ) = 11
    suits (Card _ _ BJ) = 10 --the rest of the cards
    -- Default
    suits (Card _ (Other suits) _) = suits
    suits (Card _ J _) = 10
    suits (Card _ Q _) = 11
    suits (Card _ K _) = 12
    suits (Card _ A _) = 13 --or 1?

  -- Show
  instance Show Suit where
    show Clubs = "♣︎"
    show Diamonds = "♦︎"
    show Hearts = "❤︎"
    show Spades = "♠︎"

  instance Show suits where
    show (Other suits) = show(suits)
    show J = "Jack"
    show Q = "Queen"
    show K = "King"
    show A = "Ace"

  instance Show PlayingCard where
    show (Card suit suits None) = "[" ++ show(suit) ++ show(suits) ++ "]"
    show (Card suit suits BJ) = "[BJ:" ++ show(suit) ++ show(suits) ++ "]"

  -- Eq
  instance Eq Suit where
    (==) Diamonds Diamonds = True
    (==) Clubs Clubs = True
    (==) Spades Spades = True
    (==) Hearts Hearts = True
    (==) _ _ = False

  instance Eq PlayingCard where
    (==) c1@(Card s1 _ _) c2@(Card s2 _ _) = (suits c1 == suits c2) && (s1 == s2)

  instance Ord PlayingCard where
    (<=) c1 c2 = (suits c1 <= suits c2)

  -- data
  data Suit = Clubs
            | Spades
            | Hearts
            | Diamonds

  data suits = Other Int 
             | J
             | Q
             | K
             | A

  data PlayingCard = Card Suit suits Game
