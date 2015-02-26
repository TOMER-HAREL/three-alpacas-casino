module Games.BlackJack where

  import Game
  import Card
  import Hand


  -- integrate hit, stand, double, split

  -- compare dealer hand and player hand

  {- FUNCTIONS -}

  printHand :: PlayingHand -> IO ()
  printHand hand = do
    putStrLn $ "Your hand: " ++ (show hand) ++ "."
    putStrLn $ "Hand value " ++ (show (sumOfHand hand::Int)) ++ "."


  -- printGamestate :: [PlayingHand] -> IO ()
  -- printGamestate (hand:rest) = do
  --   putStrLn show (hand)

  {-
  victory :: GameState -> Bool
  victory
          | hand > dealer && hand < 22 = True
          | hand > dealer && hand >= 22 = False
          | hand < dealer = False

  playMove :: GameState -> Move -> IO PlayingHand
  playMove hit = drawCardFromDeck addCardToHand
  playMove stand  =
  playMove double =
  playMove split ?
  -}
