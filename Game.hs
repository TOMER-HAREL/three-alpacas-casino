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
