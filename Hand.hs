module Hand where

  import Card

  {- CLASSES -}

  class HandValue a where
    sumOfHand :: a -> Int
    numberOfCards :: a -> Int
    maximumNumberOfCards :: a -> Int

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
  -- instance Ord PlayingHand where

  {- FUNCTIONS -}

  {-
    TODO
    PURPOSE: Return the card at the supplied position
    HINT: !!
  -}
  cardAtPosition :: PlayingHand -> Int -> PlayingCard
  cardAtPosition (Hand cards) position = cards !! position
  
  -- snyggast så ? fåååår jag puuusha :D:D:D:D:D:D:D:D:D:D:DD:D:D:D:D:D:D
  --
  -- NAJS, yep! cleaaant! heheheh jaaaaaa, gärna med vår text :D ;;DD 8=====================D~<------ 0:heheheheh hahahaha PUSHA NU TOM! WOWOWOWO VÅÅÅÅGAR INTE m
  {-
    TODO
    PURPOSE: Remove the card and return the new hand.
  -}
  removeCardAtPosition :: Int -> PlayingHand -> PlayingHand
  removeCardAtPosition position hand = undefined
