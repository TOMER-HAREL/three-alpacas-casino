module Deck where

  import Card
  import System.Random
  import Test.HUnit
  import Data.List

  {-
    REPRESENTATION CONVENTION: Deck represents a card deck containing 52 playing cards. EmptyDeck represents a card deck with no cards.
    REPRESENTATION INVARIANT: a deck with playing cards must contain at least 1 card and at most 52 cards.
  -}
  data PlayingDeck = Deck [PlayingCard]
                   | EmptyDeck

  {-
    PURPOSE: convert every card in a deck into a string and show i nicely.
  -}
  instance Show PlayingDeck where
    show (Deck []) = ""
    show (Deck (card:rest)) = show card ++ show (Deck rest)

  {-
    PURPOSE: check if decks are equal
  -}
  instance Eq PlayingDeck where
    (==) (Deck []) (Deck []) = True
    (==) (Deck (card:deck)) (Deck (cardb:deckb)) = card == cardb && (Deck deck) == (Deck deckb)
    (==) _ _ = False

  {-
    createEmptyDeck game
    PURPOSE: Create a playingCard deck with 52 cards, unshuffled.
    PRE:  true
    POST: a deck with 52 playing cards.
    SIDE EFFECTS: none
    EXAMPLES: createEmptyDeck = [AS][2S][3S][4S][5S][6S][7S][8S][9S][10S][JS][QS][KS][AC][2C][3C][4C]
    [5C][6C][7C][8C][9C][10C][JC][QC][KC][AD][2D][3D][4D][5D][6D][7D][8D][9D][10D][JD][QD][KD][AH][2H]
    [3H][4H][5H][6H][7H][8H][9H][10H][JH][QH][KH]
  -}
  createEmptyDeck :: PlayingDeck
  createEmptyDeck = (Deck [(Card Spades A) .. (Card Hearts K)])

  {-
    PURPOSE: return cards from a deck
  -}
  cardsFromDeck :: PlayingDeck -> [PlayingCard]
  cardsFromDeck (Deck cards) = cards

  {-

    TODO
    shuffleList
    PURPOSE: Shuffle supplied list
    CREDITS: http://stackoverflow.com/questions/9877969/haskell-functions-to-randomly-order-a-list-not-working-properly-homework-begin

    -}

  shuffleList :: StdGen -> [a] -> [a]
  shuffleList _ []   = []
  shuffleList gen xs =
    let
      (n,newGen) = randomR (0,length xs -1) gen
      front = xs !! n
    in
      front : shuffleList newGen (take n xs ++ drop (n+1) xs)

  {-
    PURPOSE: Shuffle supplied deck
    TODO: Make it random for every shuffle, unix-timestamp?
  -}
  shuffleDeck :: PlayingDeck -> PlayingDeck
  shuffleDeck (Deck cards) = (Deck (shuffleList (mkStdGen 18234891023849012) cards))

  {-
    PURPOSE: sort a deck
  -}
  sortDeck :: PlayingDeck -> PlayingDeck
  sortDeck (Deck cards) = (Deck (sort cards))

  {-
    drawCardFromDeck deck
    PURPOSE: Draw one card from the top of the deck, if there's no more cards
      return InvisibleCard
    PRE: true
    POST: The card that is on the top of the deck. If there is no cards the function returns the InvisibleCard.
    SIDE EFFECTS: none
    EXAMPLES: drawCardFromDeck (Deck [(Card Diamonds A), (Card Spades (Other 10))]) = [AD]
              drawCardFromDeck EmptyDeck = []
  -}
  drawCardFromDeck :: PlayingDeck -> PlayingCard
  drawCardFromDeck EmptyDeck = InvisibleCard
  drawCardFromDeck (Deck (card:_)) = card

  {-
    removeTopCardFromDeck deck
    PURPOSE: Remove drawn card from deck
    PRE:  true
    POST: a deck with the top card removed
    SIDE EFFECTS: none
    EXAMPLES:removeTopCardFromDeck (Deck [(Card Spades A),(Card Spades (Other 2)),
    (Card Spades (Other 3)),(Card Spades (Other 4))]) = [2S][3S][4S]
  -}
  removeTopCardFromDeck :: PlayingDeck -> PlayingDeck
  removeTopCardFromDeck (Deck (topcard:deck)) = (Deck deck)

  {- TESTS -}

  testDeck :: PlayingDeck
  testDeck = (Deck
      [Card Spades A,Card Spades (Other 2),Card Spades (Other 3),
       Card Spades (Other 4),Card Spades (Other 5),Card Spades (Other 6),
       Card Spades (Other 7),Card Spades (Other 8),Card Spades (Other 9),
       Card Spades (Other 10),Card Spades J,Card Spades Q,Card Spades K,
       Card Clubs A,Card Clubs (Other 2),Card Clubs (Other 3),
       Card Clubs (Other 4),Card Clubs (Other 5),Card Clubs (Other 6),
       Card Clubs (Other 7),Card Clubs (Other 8),Card Clubs (Other 9),
       Card Clubs (Other 10),Card Clubs J,Card Clubs Q,Card Clubs K,
       Card Diamonds A,Card Diamonds (Other 2),Card Diamonds (Other 3),
       Card Diamonds (Other 4),Card Diamonds (Other 5),
       Card Diamonds (Other 6),Card Diamonds (Other 7),
       Card Diamonds (Other 8),Card Diamonds (Other 9),
       Card Diamonds (Other 10),Card Diamonds J,Card Diamonds Q,
       Card Diamonds K,Card Hearts A,Card Hearts (Other 2),
       Card Hearts (Other 3),Card Hearts (Other 4),Card Hearts (Other 5),
       Card Hearts (Other 6),Card Hearts (Other 7),Card Hearts (Other 8),
       Card Hearts (Other 9),Card Hearts (Other 10),Card Hearts J,
       Card Hearts Q,Card Hearts K])

  testCreateEmptyDeck = TestCase $ assertBool "createEmptyDeck" ((createEmptyDeck) == testDeck)
  testDrawCardFromDeck = TestCase $ assertBool "drawCardFromDeck" ((drawCardFromDeck testDeck) == (Card Spades A))


  testListDeck = TestList [testCreateEmptyDeck, testDrawCardFromDeck]
