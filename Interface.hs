module Interface where

  _WIN_WIDTH :: Int
  _WIN_WIDTH = 128

  _WIN_HEIGHT :: Int
  _WIN_HEIGHT = 32

  printLnCenter :: (Show a) => a -> IO ()
  printLnCenter line =
    let
      numberOfSpaces = (_WIN_WIDTH `div` 2) - ((length (show line)) `div` 2)
    in
      putStrLn $ (map (\_ -> ' ') [1 .. numberOfSpaces]) ++ (show line)

  printDivider :: IO ()
  printDivider = putStrLn $ repeatCharacter '-' _WIN_WIDTH

  printSpace :: Int -> IO ()
  printSpace n = putStr (repeatCharacter '\n' n)

  repeatCharacter :: Char -> Int -> String
  repeatCharacter character n = map (\_ -> character) [1 .. n]
