module Player where

  import Hand

  data Role = Player
            | Dealer

  data GamePlayer = Player Status PlayingHand Role
