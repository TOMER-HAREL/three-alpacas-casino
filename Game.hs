module Game where

  {- DATA -}

  data Game = None
            | BJ
            | GF

  {- INSTANCES -}

  instance Show Game where
    show BJ = "Black Jack"
    show GF = "Go Fish"
    show None = "Undefined Game"

  {- FUNCTIONS -}

  {-
    TODO
    PURPOSE: Return every game that we're adding, may use it in a list
      at the Playboy Casino home screen.
  -}
  everyGame :: [Game]
  everyGame = undefined

  {-
    TODO
    PURPOSE: Return the number of players you're required to be to play a certain
      game.
  -}
  minimumOfPlayers :: Game -> Int
  minimumOfPlayers None = 0
  minimumOfPlayers BJ = undefined
  minimumOfPlayers GF = undefined
