module Game where

  import Player
  import Card
  import Deck
  import Hand
  import qualified Test.HUnit as T

  {-
    class GameValue
    PURPOSE: being able to get a value of a type, in our case we use this to extend
      PlayingCard, PlayingDeck and PlayingHand. Instead of writing new function
      declarations for every game and using different names we use this instead.
      Generic class GameValue will have an instance in every cardGame made as
      the value of cards differs.
  -}
  class GameValue a where
    valueOf :: a -> Int

  {-
    class GameEq
    PURPOSE: as each cardGame differs when it comes to check for equality in cards
      we decided to specify this class and declare an instance for it in new cardGames
      instead of writing new functions with different names in each game, and the possibility
      to check if a data type is of GameEq results in understandable code.
  -}
  class GameEq a where
    (===) :: a -> a -> Bool
    (/==) :: a -> a -> Bool
    (/==) a b = not(a === b)

  {-
    REPRESENTATION CONVENTION: Game denotes an identifier for each cardGame in the casino
    REPRESENTATION INVARIANT: None means that a game is undefined and should not be used except
      in certain debug environments.
      It's enumerable and may be checked for equality.
  -}
  data Game = None
            | BJ
            | P5 deriving(Enum, Eq)

  {-
    REPRESENTATION CONVENTION: GameStatus denotes the status of a game.
      - Red denotes that the game is over.
      - Green denotes that the game is still running and should continute.
      - Yellow string denotes borderline cases where Red and Green isn't enough. Messaging between
          functions for example.
    REPRESENTATION INVARIANT: Yellow string should have it's string in uppercase as standard and should
      consist of letters in the alphabet as well as numbers.
  -}
  data GameStatus = Green
                  | Yellow String
                  | Red

  {-
    REPRESENTATION CONVENTION: Denotes all the data in a certain cardGame,
      - [GamePlayers] The sharks and dealers that are still in the game.
      - Playingdeck is the deck used inside of the game
      - GameStatus represents the status of the game, see CC for GameStatus
      DeadGameState is a placeholder for a GameState that isn't in use.
    REPRESENTATION INVARIANT: PlayingDeck may contain multiple deck if necessary,
      gamePlayers has to be at least two, a dealer and a shark. You may have multiple dealers
      if the game has support for it.
  -}
  data GameState = GState [GamePlayer] PlayingDeck GameStatus
                 | DeadGameState

  {-
    show game
    PURPOSE: print the name of the Game datatype.
    PRE: True
    POST: a string of the games name.
    EXAMPLES:
      show BJ == "Black Jack"
      show None == "Undefined Game"
  -}
  instance Show Game where
    show BJ = "Black Jack"
    show P5 =  "Poker"
    show None = "Undefined Game"

  {-
    show gamestatus
    PURPOSE: print the datatype GameStatus
    PRE: True
    POST: A string that denotes the status.
    EXAMPLES:
      show Red == "RED"
      show (Yellow "DEALER") == "[YELLOW:DEALER]"
  -}
  instance Show GameStatus where
    show Red = "RED"
    show Green = "GREEN"
    show (Yellow status) = "[YELLOW:" ++ status ++ "]"

  {-
    show gameState
    PURPOSE: print a summary of the data inside of gameState.
    PRE: True
    POST: A string of the deck, sharks, dealers and the status.
    EXAMPLES:
      show DeadGameState == "Dead Gamestate"
      show gamestate == "Dealer: [Dealer, Undefined Status] Empty Hand,
                         Player: [Shark, Undefined Status] Empty Hand,
                         deck consists of [QS][5S][8D][2H][8C][6H][8S][AH]
                                          [4D][7C][9C][3C][8H][KH][JH][10C]
                                          [7H][3D][9H][10H][4H][2C][AD][3S]
                                          [9S][5D][AS][KS][JS][5C][3H][6D]
                                          [4C][QH][5H][2D][2S][KD][9D][7S]
                                          [10D][4S][AC][6C][QC][JC][6S][QD]
                                          [7D][KC][10S][JD]
                         and the status is GREEN"
        gamestate = GState
              [P.Player EmptyHand P.Dealer P.UndefinedState, P.Player EmptyHand P.Shark P.UndefinedState]
              (Deck [Card Spades Q,Card Spades (Other 5),Card Diamonds (Other 8), Card Hearts (Other 2),Card Clubs (Other 8),Card Hearts (Other 6), Card Spades (Other 8),Card Hearts A,Card Diamonds (Other 4), Card Clubs (Other 7),Card Clubs (Other 9),Card Clubs (Other 3), Card Hearts (Other 8),Card Hearts K,Card Hearts J, Card Clubs (Other 10),Card Hearts (Other 7), Card Diamonds (Other 3),Card Hearts (Other 9), Card Hearts (Other 10),Card Hearts (Other 4),Card Clubs (Other 2), Card Diamonds A,Card Spades (Other 3),Card Spades (Other 9), Card Diamonds (Other 5),Card Spades A,Card Spades K,Card Spades J, Card Clubs (Other 5),Card Hearts (Other 3),Card Diamonds (Other 6), Card Clubs (Other 4),Card Hearts Q,Card Hearts (Other 5), Card Diamonds (Other 2),Card Spades (Other 2),Card Diamonds K, Card Diamonds (Other 9),Card Spades (Other 7), Card Diamonds (Other 10),Card Spades (Other 4),Card Clubs A, Card Clubs (Other 6),Card Clubs Q,Card Clubs J, Card Spades (Other 6),Card Diamonds Q,Card Diamonds (Other 7), Card Clubs K,Card Spades (Other 10),Card Diamonds J])
              Game.Green
  -}
  instance Show GameState where
    show DeadGameState = "Dead Gamestate"
    show (GState [] deck status) = "deck consists of " ++ show(deck) ++ " and the status is " ++ show(status)
    show (GState (dealer@(Player _ Dealer _):rest) deck status) = "Dealer: " ++ show(dealer) ++ ", " ++ show(GState rest deck status)
    show (GState (player:rest) deck status) = "Player: " ++ show(player) ++ ", " ++ show(GState rest deck status)

  {-
    statusA == statusB,
      statusA /= statusB
    PURPOSE: compare gamestatuses
    PRE: true
    POST: a boolean value that denotes if the gamestatus are equal
    EXAMPLES:
      Red /= Green == True
      ((Yellow "A") == (Yellow "B")) == False
      ((Yellow "A") == (Yellow "A")) == True

  -}
  instance Eq GameStatus where
    (==) Red Red = True
    (==) Green Green = True
    (==) (Yellow valueA) (Yellow valueB) = valueA == valueB
    (==) _ _ = False

  {-
    statusForGameState gamestate
    PURPOSE: return status of a gamestate
    PRE: true
    POST: the status of the gamestate.
    SIDE EFFECTS: none
    EXAMPLES:
  -}
  statusForGameState :: GameState -> GameStatus
  statusForGameState (GState _ _ status) = status

  {-
    everygame
    PURPOSE: Return every game that we're adding
    PRE: true
    POST: a list with all games
    SIDE EFFECTS: none
    EXAMPLES: everyGame = [Black Jack, Poker]
  -}
  everyGame :: [Game]
  everyGame = [BJ ..]

  {-
    playersWithRoleInGameState gamestate needle
    PURPOSE: find all players in a game with the same playerole
    PRE: true
    POST: a list with players containing given playerrole
    SIDE EFFECTS: none
    EXAMPLES: (playersWithRoleInGameState (GState [(Player testHand Dealer UndefinedState),
                                            (Player testHand Shark UndefinedState)] testDeck Green) Shark
                                            == [(Player testHand Shark UndefinedState)])
  -}
  playersWithRoleInGameState :: GameState -> PlayerRole -> [GamePlayer]
  playersWithRoleInGameState (GState players _ _) needle =
    filter (\(Player _ role _) -> role == needle) players

  {-
    deckInGameState gamestate
    PURPOSE: return deck from gamestate
    PRE: true
    POST: deck from given gamestate
    SIDE EFFECTS: none
    EXAMPLES: deckInGameState (GState [(Player testHand Shark UndefinedState)] (Deck [(Card Spades A),
                                        (Card Spades (Other 2)),(Card Spades (Other 3))]) Green)
                                          == (Deck [(Card Spades A),(Card Spades (Other 2)),(Card Spades (Other 3))]))
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

  {- TESTS -}
  teststatusForGameState = T.TestCase $ T.assertBool "statusForGameState" (statusForGameState (GState [Player testHand Shark UndefinedState] testDeck Green) == Green)
  testeveryGame = T.TestCase $ T.assertBool "everyGame" (everyGame == [BJ, P5])
  testplayersWithRoleInGameState = T.TestCase $ T.assertBool "playersWithRoleInGameState" (playersWithRoleInGameState (GState [(Player testHand Dealer UndefinedState),(Player testHand Shark UndefinedState)] testDeck Green) Shark == [(Player testHand Shark UndefinedState)])
  testdeckInGameState = T.TestCase $ T.assertBool "deckInGameState" (deckInGameState (GState [(Player testHand Shark UndefinedState)] (Deck [(Card Spades A),(Card Spades (Other 2)),(Card Spades (Other 3))]) Green) == (Deck [(Card Spades A),(Card Spades (Other 2)),(Card Spades (Other 3))]))
  testgameCount = T.TestCase $ T.assertBool "gameCount" (gameCount == 2)
  testvalidGameEnum = T.TestCase $ T.assertBool "validGameEnum" (validGameEnum 2 == True)

  testlistGame = T.TestList [teststatusForGameState,
                              testeveryGame,
                              testdeckInGameState,
                              testplayersWithRoleInGameState,
                              testgameCount,
                              testvalidGameEnum]
