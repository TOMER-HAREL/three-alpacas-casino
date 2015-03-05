module Game where

  import Player
  import Card
  import Deck
  import Hand
  import qualified Test.HUnit as T

  

  class GameValue a where
    valueOf :: a -> Int

  class GameEq a where
    (===) :: a -> a -> Bool
    (/==) :: a -> a -> Bool
    (/==) a b = not(a === b)

  data Game = None
            | BJ
            | P5 deriving(Enum, Eq)

  data GameStatus = Green
                  | Yellow String
                  | Red

  data GameState = GState [GamePlayer] PlayingDeck GameStatus
                 | FuckYouState

  instance Show Game where
    show BJ = "Black Jack"
    show P5 =  "Poker"
    show None = "Undefined Game"

  instance Show GameState where
    show (GState [] deck _) = "deck consists of " ++ show(deck)
    show (GState (dealer@(Player _ Dealer _):rest) deck status) = "Dealer: " ++ show(dealer) ++ ", " ++ show(GState rest deck status)
    show (GState (player:rest) deck status) = "Player: " ++ show(player) ++ ", " ++ show(GState rest deck status)

  instance Eq GameStatus where
    (==) Red Red = True
    (==) Green Green = True
    (==) (Yellow valueA) (Yellow valueB) = valueA == valueB
    (==) _ _ = False

  {-
    PURPOSE: return status of a gamestate
  -}
  statusForGameState :: GameState -> GameStatus
  statusForGameState (GState _ _ status) = status

  {-
    everygame
    PURPOSE: Return every game that we're adding, may use it in a list
      at the Three Alpacas homescreen.
    PRE: true
    POST: a list with all games
    SIDE EFFECTS: none
    EXAMPLES: everyGame = [Black Jack, Poker]
  -}
  everyGame :: [Game]
  everyGame = [BJ ..] --thanks to deriving(Enum) we can do this.

  {-

  TODO CC

  -}
  playersWithRoleInGameState :: GameState -> PlayerRole -> [GamePlayer]
  playersWithRoleInGameState (GState players _ _) needle =
    filter (\(Player _ role _) -> role == needle) players

  {-

    PURPOSE: return deck from gamestate
    TODO
  -}
  deckInGameState :: GameState -> PlayingDeck
  deckInGameState (GState _ deck _) = deck

  {-
    gameCount
    PURPOSE: return an integer that denotes the number of games available.
    PRE: true
    POST: the number of available games
    SIDE EFFECTS: none
    EXAMPLES: gameCount = 2
  -}

  gameCount :: Int
  gameCount = length everyGame

  {-
    validGameEnum
    PURPOSE: Check if a number is mapped to a game.
    PRE: true
    POST: bool that tells you if a game at given index exists
    SIDE EFFECTS: none
    EXAMPLES:  validGameEnum 1 = True
  -}

  validGameEnum :: Int -> Bool
  validGameEnum enum = (enum <= gameCount) && (enum > 0)

  {-
    printGameTable games
    PURPOSE: Provide the player with a list of all the games available.
    PRE: true
    POST: TODO
    SIDE EFFECTS: TODO
    EXAMPLES:  TODO
  -}

  {- TESTS -}

  testeveryGame = T.TestCase $ T.assertBool "everyGame" (everyGame == [BJ, P5])
  testplayersWithRoleInGameState = T.TestCase $ T.assertBool "playersWithRoleInGameState" (playersWithRoleInGameState (GState [(Player testHand Dealer UndefinedState),(Player testHand Shark UndefinedState)] testDeck Green) Shark == [(Player testHand Shark UndefinedState)])
  testdeckInGameState = T.TestCase $ T.assertBool "deckInGameState" (deckInGameState (GState [(Player testHand Shark UndefinedState)] (Deck [(Card Spades A),(Card Spades (Other 2)),(Card Spades (Other 3))]) Green) == (Deck [(Card Spades A),(Card Spades (Other 2)),(Card Spades (Other 3))]))
  testgameCount = T.TestCase $ T.assertBool "gameCount" (gameCount == 2)
  testvalidGameEnum = T.TestCase $ T.assertBool "validGameEnum" (validGameEnum 2 == True)

  testlistGame = T.TestList [testeveryGame,
                              testdeckInGameState,
                              testplayersWithRoleInGameState,
                              testgameCount,
                              testvalidGameEnum]
