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
