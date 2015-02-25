module Hand where

  import Card
  import Game

  {- CLASSES -}

  class HandValue a where
    sumOfHand :: a -> Int
    numberOfCards :: a -> Int
    maximumNumberOfCards :: a -> Int

  {- DATA -}

  data PlayingHand = Hand [PlayingCard] Game

  {- INSTANCES -}

  instance Show PlayingHand where
    show (Hand [] _) = []
    show (Hand (card:xs) game) = show(card) ++ " " ++ show(Hand xs game)

  instance HandValue PlayingHand where
    sumOfHand (Hand [] _) = 0
    sumOfHand (Hand (card:rest) game) = (valueOf card) + (sumOfHand (Hand rest game))

  -- TODO:
  -- instance Ord PlayingHand where

  {- FUNCTIONS -}

  {-
    TODO
    PURPOSE: Return the card at the supplied position
    HINT: !!
  -}
  cardAtPosition :: PlayingHand -> Int -> PlayingCard
  cardAtPosition (Hand cards _) position = cards !! position

  {-
    TODO
    PURPOSE: Remove the card and return the new hand.
  -}
  removeCardAtPosition :: Int -> PlayingHand -> PlayingHand
  removeCardAtPosition position hand = undefined
