module Hand where

  import Card

  {-
    CLASSES
  -}
  class HandValue a where
    sumOfHand :: a -> Int

  {-
    DATA
  -}
  data PlayingHand = Hand [PlayingCard]

  {-
    INSTANCES
  -}
  instance Show PlayingHand where
    show (Hand []) = []
    show (Hand (card:xs)) = show(card) ++ " " ++ show(Hand xs)

  instance HandValue PlayingHand where
    sumOfHand (Hand []) = 0
    sumOfHand (Hand (card:rest)) = (valueOf card) + (sumOfHand (Hand rest))
