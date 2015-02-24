module Hand where

  import Card

  data PlayingHand = Hand [PlayingCard]

  instance Show PlayingHand where
    show (Hand []) = []
    show (Hand (card:xs)) = show(card) ++ " " ++ show(Hand xs)
