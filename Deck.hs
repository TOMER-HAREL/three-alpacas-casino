module Deck where

  import Card
  import Game
  import System.Random
  import Test.HUnit

  {- DATA -}

  {-  REPRESENTATION CONVENTION: ... description of how the datatype represents data ...
      REPRESENTATION INVARIANT:  ... requirements on elements of the datatype that the code preserves at all times ...
     -}

  data PlayingDeck = Deck [PlayingCard]
                   | EmptyDeck

  {- INSTANCES -}

  {-
    PURPOSE: convert every card in a deck into a string and show i nicely.
  -}
  instance Show PlayingDeck where
    show (Deck []) = ""
    show (Deck (card:rest)) = show card ++ show (Deck rest)

  instance Eq PlayingDeck where
    (==) (Deck []) (Deck []) = True
    (==) (Deck (card:deck)) (Deck (cardb:deckb)) = card == cardb && (Deck deck) == (Deck deckb)
    (==) _ _ = False

  {- FUNCTIONS -}

  {-
    createEmptyDeck game
    PURPOSE: Create a playingCard deck with 52 cards, unshuffled.
    PRE:  ... pre-condition on the arguments ...
    POST: ... post-condition on the result, in terms of the arguments ...
    SIDE EFFECTS: ... if any, including exceptions ...
    EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ..
  -}
  createEmptyDeck :: PlayingDeck
  createEmptyDeck = (Deck [(Card Spades A) .. (Card Hearts K)])

  {-
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
  shuffleDeck (Deck cards) = (Deck (shuffleList (mkStdGen 1023012301230) cards))

  {-
    drawCardFromDeck deck
    PURPOSE: Draw one card from the top of the deck, if there's no more cards
      return InvisibleCard
    PRE:  ... pre-condition on the arguments ...
    POST: ... post-condition on the result, in terms of the arguments ...
    SIDE EFFECTS: ... if any, including exceptions ...
    EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ..
  -}
  drawCardFromDeck :: PlayingDeck -> PlayingCard
  drawCardFromDeck EmptyDeck = InvisibleCard
  drawCardFromDeck (Deck (card:_)) = card

  {-
    removeTopCardFromDeck deck
    PURPOSE: Remove drawn card from deck
    PRE:  ... pre-condition on the arguments ...
    POST: ... post-condition on the result, in terms of the arguments ...
    SIDE EFFECTS: ... if any, including exceptions ...
    EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ..
  -}
  removeTopCardFromDeck :: PlayingDeck -> PlayingDeck
  removeTopCardFromDeck (Deck (topcard:deck)) = (Deck deck)

  {- TESTS -}

  testDeck :: PlayingDeck
  testDeck =  Deck [Card Spades A,Card Spades (Other 2),
          Card Spades (Other 3),Card Spades (Other 4),
          Card Spades (Other 5),Card Spades (Other 6),
          Card Spades (Other 7),Card Spades (Other 8),
          Card Spades (Other 9),Card Spades (Other 10),
          Card Spades J,Card Spades Q,Card Spades K,Card Clubs A,
          Card Clubs (Other 2),Card Clubs (Other 3),
          Card Clubs (Other 4),Card Clubs (Other 5),
          Card Clubs (Other 6),Card Clubs (Other 7),
          Card Clubs (Other 8),Card Clubs (Other 9),
          Card Clubs (Other 10),Card Clubs J,Card Clubs Q,
          Card Clubs K,Card Diamonds A,Card Diamonds (Other 2),
          Card Diamonds (Other 3),Card Diamonds (Other 4),
          Card Diamonds (Other 5),Card Diamonds (Other 6),
          Card Diamonds (Other 7),Card Diamonds (Other 8),
          Card Diamonds (Other 9),Card Diamonds (Other 10),
          Card Diamonds J,Card Diamonds Q,Card Diamonds K,
          Card Hearts A,Card Hearts (Other 2),Card Hearts (Other 3),
          Card Hearts (Other 4),Card Hearts (Other 5),
          Card Hearts (Other 6),Card Hearts (Other 7),
          Card Hearts (Other 8),Card Hearts (Other 9),
          Card Hearts (Other 10),Card Hearts J,Card Hearts Q,
          Card Hearts K]

  testCreateEmptyDeck = TestCase $ assertBool "createEmptyDeck" ((createEmptyDeck) == testDeck)
  testDrawCardFromDeck = TestCase $ assertBool "drawCardFromDeck" ((drawCardFromDeck testDeck) == (Card Spades A))


  testListDeck = TestList [testCreateEmptyDeck, testDrawCardFromDeck]
