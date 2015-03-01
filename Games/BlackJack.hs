module Games.BlackJack where

  import qualified Test.HUnit as T
  import Game
  import Card
  import Hand
  import Player
  import Deck


  -- integrate hit, stand, double, split

  -- compare dealer hand and player hand

  {- FUNCTIONS -}

  printHand :: PlayingHand -> IO ()
  printHand hand = do
    putStrLn $ "Your hand: " ++ (show hand) ++ "."
    putStrLn $ "Hand value " ++ (show (sumOfHand hand::Int)) ++ "."

  states :: [PlayerState]
  states = [(State "HIT"), (State "UNKNOWN"), (State "SPLIT"), (State "STAND"), (State "DOUBLE")]

  createDeck :: PlayingDeck
  createDeck = shuffleDeck (createEmptyDeck BJ)


  performMove :: GamePlayer -> PlayingDeck -> GamePlayer
  performMove (Player hand roles (State "SPLIT") _) deck = undefined
  performMove (Player hand role (State "HIT") _) deck = (Player hand role (State "UNKNOWN") BJ)
  performMove _ deck = undefined

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

  {-
    TODO: Test cases
  -}
