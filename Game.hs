module Game where

  import Player
  import Card
  import Deck
  import Hand


  class GameValue a where
    valueOf :: a -> Int

  class GameEq a where
    (===) :: a -> a -> Bool
    (/==) :: a -> a -> Bool
    (/==) a b = not(a === b)

  data Game = None
            | BJ
            | TX
            | P5
            | GF deriving(Enum, Eq)

  data GameState = GState [GamePlayer] PlayingDeck

  instance Show Game where
    show BJ = "Black Jack"
    show GF = "Go Fish"
    show TX = "Texas Hold'Em"
    show P5 =  "Poker"
    show None = "Undefined Game"

  instance Show GameState where
    show (GState [] deck) = "deck consists of " ++ show(deck)
    show (GState (dealer@(Player _ Dealer _):rest) deck) = "Dealer: " ++ show(dealer) ++ ", " ++ show(GState rest deck)
    show (GState (player:rest) deck) = "Player: " ++ show(player) ++ ", " ++ show(GState rest deck)

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
    PURPOSE: return players in provided gamestate.
  -}
  playersInGameState :: GameState -> [GamePlayer]
  playersInGameState (GState players _) = players

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


  printGameTable :: [Game] -> IO ()
  printGameTable [] = putStrLn "Playboy Casino is out of poison."
  printGameTable games =
    let
      printGameTable' :: [Game] -> Int -> IO ()
      printGameTable' [] _ = return ()
      printGameTable' (game:rest) acc = do
        putStrLn ("[" ++ show(acc) ++ "] " ++ show(game))
        printGameTable' rest (acc + 1)
    in
      do
        putStrLn("[n] GAME")
        putStrLn("-------------------------------")
        printGameTable' games 1
        putStr("Please pick your poison [1 - " ++ show(gameCount) ++ "]")
