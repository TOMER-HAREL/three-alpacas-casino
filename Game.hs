module Game (Game(..)) where

  data Game = None
            | BJ

  instance Show Game where
    show BJ = "Black Jack"
    show None = "Undefined Game"
