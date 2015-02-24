import qualified GBlackJack as BJ
import qualified GGoFish as GF

import Card
import Hand
import Game

main :: IO ()
main = do
  putStrLn "Welcome to Playboy Casino."
  putStrLn $ show (Hand [(Card Diamonds A BJ),
                         (Card Spades (Other 5) BJ),
                         (Card Clubs K BJ),
                         (Card Diamonds (Other 2) BJ)])
