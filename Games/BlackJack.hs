module Games.BlackJack where

  import qualified Test.HUnit as T
  import Game
  import Card
  import Hand
  import Player
  import Deck

  -- compare dealer hand and player hand

  data GameState = GState [GamePlayer] PlayingDeck

  instance GameValue PlayingCard where
    valueOf (Card _ (Other value)) = value
    valueOf (Card _ A) = 11
    valueOf (Card _ _) = 10

  instance GameValue PlayingHand where
    valueOf (Hand []) = 0
    valueOf (Hand (card:rest)) = (valueOf card) + (valueOf (Hand rest))

  instance Show GameState where
    show (GState [] deck) = "No players BJ"

  instance Ord PlayingCard where
    (<=) InvisibleCard InvisibleCard = True
    (<=) cardA cardB = valueOf cardA <= valueOf cardB

  -- instance Show C.PlayingCard where
  --   show (C.Card _ C.A _) = "21"

  {- FUNCTIONS -}

  printHand :: PlayingHand -> IO ()
  printHand hand = do
    putStrLn $ "Your hand: " ++ (show hand) ++ "."
    putStrLn $ "Hand value " ++ (show (valueOf hand::Int)) ++ "."


  {-
    PURPOSE: list every possible state a blackjack player could have
  -}
  states :: [PlayerState]
  states = [(State "HIT"), (State "UNKNOWN"), (State "SPLIT"), (State "STAND"), (State "DOUBLE")]

  {-
    PURPOSE:
  -}
  dealCard :: PlayingDeck -> GamePlayer -> IO GamePlayer
  dealCard EmptyDeck player = return player
  dealCard deck (Player hand role state) = return (Player (addCardToHand hand (drawCardFromDeck deck)) role state)

  main :: IO ()
  main = do
    putStrLn ("Welcome to " ++ show(BJ))
    deck <- createDeck
    player <- createBJShark
    newPlayer <- dealCard deck player
    newDeck <- return (removeTopCardFromDeck deck)
    putStrLn ("New hand: " ++ show (getHand newPlayer))
    putStrLn ("New deck: " ++ show (newDeck))
    -- player <- (Player (Hand []) Shark (State "") BJ)
    -- putStrLn ("PlayerHand: " ++ show(player))

  getHand :: GamePlayer -> PlayingHand
  getHand (Player hand _ _) = hand

  createBJShark :: IO GamePlayer
  createBJShark = return (createShark)

  {-
    PURPOSE: create a blackjack deck, consists of one deck.
  -}
  createDeck :: IO PlayingDeck
  createDeck = return (shuffleDeck (createEmptyDeck))


  performMove :: GamePlayer -> PlayingDeck -> GamePlayer
  performMove (Player hand roles (State "SPLIT")) deck = undefined
  performMove (Player hand role (State "HIT")) deck = (Player hand role (State "UNKNOWN"))
  performMove _ deck = undefined

  {-
    TODO: Test cases
  -}
