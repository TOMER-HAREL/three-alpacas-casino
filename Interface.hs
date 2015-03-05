module Interface where

  import Data.Char

  {-
    _WIN_WIDTH
    PURPOSE: return the desired width of our game, defined in columns.
    PRE: true
    POST: the width of the interface
    EXAMPLES: omitted
  -}
  _WIN_WIDTH :: Int
  _WIN_WIDTH = 64

  {-
    _WIN_HEIGHT
    PURPOSE: return the desired height of our game, defined in columns.
    PRE: true
    POST: the height of the interface
    EXAMPLES: omitted
  -}
  _WIN_HEIGHT :: Int
  _WIN_HEIGHT = 32

  {-
    printLnCenter line
    PURPOSE: print a line that is centered in the interface
    PRE: the line being printed should have a length less than _WIN_WIDTH to fit, but is not
      necessary. But it will break the purpose of the function as it will clip.
    POST: a line with spaces added to center the line in the interface.
    SIDE-EFFECTS: prints a line to the terminal.
    EXAMPLES:
      printLnCenter "Hello World" will print "                          Hello World"
  -}
  printLnCenter :: (Show a) => a -> IO ()
  printLnCenter line =
    let
      numberOfSpaces = (_WIN_WIDTH `div` 2) - ((length (show line)) `div` 2)
    in
      putStrLn $ (map (\_ -> ' ') [1 .. numberOfSpaces]) ++ (show line)

  {-
    printFancyLn line
    PURPOSE: print a line that stands out in the interface, used to inform the player of a
      positive state in the game.
    PRE: same as printLnCenter
    POST: a line that is spaced above and under the line, as well as the line being centered
      in the interface and uppercased.
    SIDE-EFFECTS: prints lines to the terminal
    EXAMPLES:
      printFancyLine "You won" will print \
      "

                      YOU WON

      "
  -}
  printFancyLn :: (Show a) => a -> IO ()
  printFancyLn line = do
    printSpace 2
    printLnCenter $ map toUpper (show line)
    printSpace 2

  {-
    printDivier
    PURPOSE: print a divider that consists of '-' _WIN_WIDTH times
    PRE: true
    POST: a line of multiple '-'
    SIDE-EFFECTS: prints the divider to the terminal
    EXAMPLES:
      printDivider will print "--------------------------------------------"
  -}
  printDivider :: IO ()
  printDivider = putStrLn $ repeatCharacter '-' _WIN_WIDTH

  {-
    printSpace n
    PURPOSE: print the string '\n' n times to the terminal, which will produce one line-break
      every n
    PRE: n can't be negative, or there will be no spaces
    POST: a line that will print '\n' n specified times
    SIDE-EFFECTS: prints the breaks to the terminal
    EXAMPLES:
      printSpace 2 will print "\n\n", also seen as \
      "

      "
  -}
  printSpace :: Int -> IO ()
  printSpace n = putStr (repeatCharacter '\n' n)

  {-
    repeatCharacter character n
    PURPOSE: repeat character n-times and return it in a string
    PRE: n has to be positive.
    POST: a string of character repeated n-times.
    EXAMPLES:
      repeatCharacter '\n' 5 == "\n\n\n\n\n"
      repeatCharacter '-' 0 == ""
  -}
  repeatCharacter :: Char -> Int -> String
  repeatCharacter character n = map (\_ -> character) [1 .. n]
