module Hand where

  import Card

  {- CLASSES -}

  class HandValue a where
    sumOfHand :: a -> Int

  {- DATA -}

  data PlayingHand = Hand [PlayingCard]

  {- INSTANCES -}
  
  instance Show PlayingHand where
    show (Hand []) = []
    show (Hand (card:xs)) = show(card) ++ " " ++ show(Hand xs)

  instance HandValue PlayingHand where
    sumOfHand (Hand []) = 0
    sumOfHand (Hand (card:rest)) = (valueOf card) + (sumOfHand (Hand rest))

  -- TODO:
  instance Ord PlayingHand where

  {- FUNCTIONS -}

  {-
    TODO
    PURPOSE: Return the card at the supplied position
    HINT: !!
  -}
  cardAtPosition :: Int -> PlayingHand -> PlayingCard
  cardAtPosition position hand = undefined

  {-
    TODO
    PURPOSE: Remove the card and return the new hand.
  -}
  removeCardAtPosition :: Int -> PlayingHand -> PlayingHand
  removeCardAtPosition position hand = undefined
