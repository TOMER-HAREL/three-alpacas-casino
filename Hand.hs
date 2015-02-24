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

  data CardCount = MinimumCards Int
                 | MaximumCards Int
                 | NoLimit


  {- INSTANCES -}

  instance Show PlayingHand where
    show (Hand [] _) = []
    show (Hand (card:xs) game) = show(card) ++ " " ++ show(Hand xs game)

  instance HandValue PlayingHand where
    sumOfHand (Hand [] _) = 0
    sumOfHand (Hand (card:rest) game) = (valueOf card) + (sumOfHand (Hand rest game))

    numberOfCards (Hand cards BJ) = undefined
    maximumNumberOfCards (Hand cards BJ) = NoLimit

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

  -- snyggast så ? fåååår jag puuusha :D:D:D:D:D:D:D:D:D:D:DD:D:D:D:D:D:D
  --
  -- NAJS, yep! cleaaant! heheheh jaaaaaa, gärna med vår text :D ;;DD 8=====================D~<------ 0:heheheheh hahahaha PUSHA NU TOM! WOWOWOWO VÅÅÅÅGAR INTE m
  {-
    TODO
    PURPOSE: Remove the card and return the new hand.
  -}
  removeCardAtPosition :: Int -> PlayingHand -> PlayingHand
  removeCardAtPosition position hand = undefined
