module Deck where

  import Card
  import Game
  import Test.HUnit

  {- DATA -}

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
    PURPOSE: Create a playingCard deck with 52 cards, unshuffled.
  -}
  createEmptyDeck :: Game -> PlayingDeck
  createEmptyDeck game = (Deck (concat $ map (\suit -> (map (\value -> (Card suit value game)) [A .. K])) [Spades ..]))

  {-
    TODO
    PURPOSE: Shuffle the supplied deck
  -}
  shuffleDeck :: PlayingDeck -> PlayingDeck
  shuffleDeck deck = undefined

  {-
    PURPOSE: Draw one card from the top of the deck, if there's no more cards
      return InvisibleCard
  -}
  drawCardFromDeck :: PlayingDeck -> PlayingCard
  drawCardFromDeck EmptyDeck = InvisibleCard
  drawCardFromDeck (Deck (card:_)) = card

  {- TESTS -}
  testDeck :: PlayingDeck
  testDeck =  Deck [Card Spades A BJ,Card Spades (Other 2) BJ,
          Card Spades (Other 3) BJ,Card Spades (Other 4) BJ,
          Card Spades (Other 5) BJ,Card Spades (Other 6) BJ,
          Card Spades (Other 7) BJ,Card Spades (Other 8) BJ,
          Card Spades (Other 9) BJ,Card Spades (Other 10) BJ,
          Card Spades J BJ,Card Spades Q BJ,Card Spades K BJ,Card Clubs A BJ,
          Card Clubs (Other 2) BJ,Card Clubs (Other 3) BJ,
          Card Clubs (Other 4) BJ,Card Clubs (Other 5) BJ,
          Card Clubs (Other 6) BJ,Card Clubs (Other 7) BJ,
          Card Clubs (Other 8) BJ,Card Clubs (Other 9) BJ,
          Card Clubs (Other 10) BJ,Card Clubs J BJ,Card Clubs Q BJ,
          Card Clubs K BJ,Card Diamonds A BJ,Card Diamonds (Other 2) BJ,
          Card Diamonds (Other 3) BJ,Card Diamonds (Other 4) BJ,
          Card Diamonds (Other 5) BJ,Card Diamonds (Other 6) BJ,
          Card Diamonds (Other 7) BJ,Card Diamonds (Other 8) BJ,
          Card Diamonds (Other 9) BJ,Card Diamonds (Other 10) BJ,
          Card Diamonds J BJ,Card Diamonds Q BJ,Card Diamonds K BJ,
          Card Hearts A BJ,Card Hearts (Other 2) BJ,Card Hearts (Other 3) BJ,
          Card Hearts (Other 4) BJ,Card Hearts (Other 5) BJ,
          Card Hearts (Other 6) BJ,Card Hearts (Other 7) BJ,
          Card Hearts (Other 8) BJ,Card Hearts (Other 9) BJ,
          Card Hearts (Other 10) BJ,Card Hearts J BJ,Card Hearts Q BJ,
          Card Hearts K BJ]

  testCreateEmptyDeck = TestCase $ assertBool "createEmptyDeck" ((createEmptyDeck BJ) == testDeck)
  testDrawCardFromDeck = TestCase $ assertBool "drawCardFromDeck" ((drawCardFromDeck testDeck) == (Card Spades A BJ))


  testListDeck = TestList [testCreateEmptyDeck, testDrawCardFromDeck]
