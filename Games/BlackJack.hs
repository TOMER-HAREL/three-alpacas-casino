module Games.BlackJack where

  import qualified Test.HUnit as T
  import Game
  import Card
  import Hand
  import Player
  import Deck

  -- compare dealer hand and player hand

  data GameState = GState [GamePlayer] PlayingDeck

  instance Show GameState where
    show (GState [] deck) = "No players BJ"

  -- instance Show C.PlayingCard where
  --   show (C.Card _ C.A _) = "21"

  {- FUNCTIONS -}

  printHand :: PlayingHand -> IO ()
  printHand hand = do
    putStrLn $ "Your hand: " ++ (show hand) ++ "."
    putStrLn $ "Hand value " ++ (show (sumOfHand hand::Int)) ++ "."


  {-
    PURPOSE: list every possible state a blackjack player could have
  -}
  states :: [PlayerState]
  states = [(State "HIT"), (State "UNKNOWN"), (State "SPLIT"), (State "STAND"), (State "DOUBLE")]

  {-
    PURPOSE:
  -}
  dealCard :: PlayingDeck -> GamePlayer -> GamePlayer
  dealCard EmptyDeck player = player
  dealCard deck (Player hand role state game) = (Player (addCardToHand hand (drawCardFromDeck deck)) role state game)

  main :: IO ()
  main = do
    putStrLn ("Welcome to " ++ show(BJ))
    deck <- createDeck
    putStrLn ("Current deck: " ++ show(deck))
    -- player <- (Player (Hand []) Shark (State "") BJ)
    -- putStrLn ("PlayerHand: " ++ show(player))


  {-
    PURPOSE: create a blackjack deck, consists of one deck.
  -}
  createDeck :: IO PlayingDeck
  createDeck = do
    return (shuffleDeck (createEmptyDeck BJ))


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
