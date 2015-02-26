module Game where

  {- DATA -}

  data Game = None
            | BJ
            | TX
            | GF deriving(Enum)

  {- INSTANCES -}

  instance Show Game where
    show BJ = "Black Jack"
    show GF = "Go Fish"
    show TX = "Texas Hold'Em"
    show None = "Undefined Game"

  {- FUNCTIONS -}

  {-
    PURPOSE: Return every game that we're adding, may use it in a list
      at the Playboy Casino home screen.
  -}
  everyGame :: [Game]
  everyGame = [BJ ..] --thanks to deriving(Enum) we can do this.


  printGameTable :: [Game] -> IO ()
  printGameTable (game:rest) = undefined



  {-
    TODO
    PURPOSE: Return the number of players you're required to be to play a certain
      game.
  -}
  minimumOfPlayers :: Game -> Int
  minimumOfPlayers None = 0
  minimumOfPlayers BJ = 1
  minimumOfPlayers GF = 2
  minimumOfPlayers TX = 2
