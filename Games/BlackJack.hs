module Games.BlackJack where

  import qualified Test.HUnit as T
  import Game
  import Card
  import Hand
  import Player
  import Deck

  data GameState = GState [GamePlayer] PlayingDeck

  instance GameValue PlayingCard where
    valueOf (Card _ (Other value)) = value
    valueOf (Card _ A) = 11
    valueOf (Card _ _) = 10

  instance GameValue PlayingHand where
    valueOf (Hand []) = 0
    valueOf (Hand (card:rest)) = (valueOf card) + (valueOf (Hand rest))

  instance GameValue PlayingDeck where
    valueOf (Deck []) = 0
    valueOf (Deck (card:rest)) = (valueOf card) + (valueOf (Deck rest))

  instance GameEq PlayingCard where
    (===) cardA cardB = valueOf cardA == valueOf cardB

  instance Show GameState where
    show (GState [] deck) = "deck consists of " ++ show(deck)
    show (GState (player@(Player _ Dealer _):rest) deck) = "Dealer: " ++ show(player) ++ ", " ++ show(GState rest deck)
    show (GState (player:rest) deck) = "Player: " ++ show(player) ++ ", " ++ show(GState rest deck)

  instance Ord PlayingCard where
    (<=) InvisibleCard InvisibleCard = True
    (<=) cardA cardB = valueOf cardA <= valueOf cardB

  {-
    PURPOSE: main function to fire it all up.
  -}
  main :: IO ()
  main = do
    putStrLn ("Welcome to " ++ show(BJ))
    gameState <- playerPhase
    putStrLn (show(gameState))


  {-
    PURPOSE: the phase where the user defines the number of players and generates
      a matching gamestate for it.
  -}
  playerPhase :: IO GameState
  playerPhase = do
    putStr ("How many players are you participating? [1 - 6]: ")
    userInput <- getLine
    let numberOfPlayers = read userInput :: Int
    return (generateGameStateForPlayers numberOfPlayers)

  {-
    PURPOSE: generate a gamestate for n players and a dealer.
  -}
  generateGameStateForPlayers :: Int -> GameState
  generateGameStateForPlayers number =
    let
      generateGameStateForPlayers' :: Int -> [GamePlayer]
      generateGameStateForPlayers' 0 = []
      generateGameStateForPlayers' number = createShark : (generateGameStateForPlayers' (number - 1))
    in
      (GState (createDealer : (generateGameStateForPlayers' number)) createEmptyDeck)

  {-
    PURPOSE: print a players hand
  -}
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
    PURPOSE: deal a card to one player from a provided deck
  -}
  dealCard :: PlayingDeck -> GamePlayer -> IO GamePlayer
  dealCard EmptyDeck player = return player
  dealCard deck (Player hand role state) = return (Player (addCardToHand hand (drawCardFromDeck deck)) role state)

  {-
    PURPOSE: return the hand of a player.
  -}
  getHand :: GamePlayer -> PlayingHand
  getHand (Player hand _ _) = hand

  {-
    PURPOSE: create a blackjack deck, consists of one deck.
  -}
  createDeck :: IO PlayingDeck
  createDeck = return (shuffleDeck (createEmptyDeck))

  {-
    PURPOSE: return available states for a certain hand of cards.
  -}
  statesAvailable :: PlayingHand -> [PlayerState]
  statesAvailable (Hand (cards))
                          | length (cards) == 2 && head (cards) == last (cards) = [(State "SPLIT"), (State "Double"), (State "HIT"),(State "STAND")]
                          | otherwise = undefined

  {-
    PURPOSE: perform move for one player
  -}
  performMove :: GamePlayer -> PlayingDeck -> [GamePlayer]
  performMove (Player (Hand (card:cards)) roles (State "SPLIT")) deck = [(Player (Hand [card]) roles (State "SPLIT")),(Player (Hand cards) roles (State "SPLIT"))]
  performMove (Player hand role (State "HIT")) deck = [(Player (addCardToHand hand (drawCardFromDeck deck)) role (UndefinedState))]
  performMove (Player hand role (State "DOUBLE")) deck = [(Player (addCardToHand hand (drawCardFromDeck deck)) role (State "DOUBLE"))]
  performMove (Player hand role (State "STAND")) deck = [(Player hand role (UndefinedState))]
  performMove _ deck = undefined


  {-
    TODO: Test cases
  -}
  testBJDrawCardFromDeck = T.TestCase $ T.assertBool "testBJDrawCardFromDeck" ((createEmptyDeck) == testDeck)


  testListDeck = T.TestList [testCreateEmptyDeck, testDrawCardFromDeck]
