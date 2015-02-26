module Player where

  import Deck

  data PlayerRole = Dealer
                  | Player

  data GamePlayer = Player PlayingHand PlayerRole
