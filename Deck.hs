module Deck where

  import Card
  import System.Random
  import Test.HUnit
  import Data.List

  {-
    REPRESENTATION CONVENTION: Deck represents a card deck containing a number of cards.
      EmptyDeck represents a card deck with no cards.
    REPRESENTATION INVARIANT: a deck with playing cards must contain at least 1 card.
  -}
  data PlayingDeck = Deck [PlayingCard]
                   | EmptyDeck

  {-
   show deck
   PURPOSE: override show for the datatype PlayingDeck, show cards inline.
   PRE: True
   POST: a string that shows all the cards in a deck, if EmptyDeck a message.
   EXAMPLES:
     show (Deck [Card Spades A,Card Spades (Other 2),Card Spades (Other 3)]) == "[AS][2S][3S]"
     show EmptyDeck == "Empty Deck"
     show (Deck []) == ""
 -}
  instance Show PlayingDeck where
    show EmptyDeck = "Empty Deck"
    show (Deck []) = ""
    show (Deck (card:rest)) = show card ++ show (Deck rest)

  {-
    deckA == deckB
    PURPOSE: compare two decks and check if equal (or not) based on (==) for playingCard
      matches by suit and value that would be
    PRE: true
    POST: a boolean value that denotes if deckA is equal to deckB
    EXAMPLES:
      (deckA == deckB) == False
      (deckA /= deckB) == True
        deckA = (Deck [Card Spades A,Card Spades (Other 2),Card Spades (Other 3)])
        deckB = (Deck [Card Spades K,Card Spades (Other 2),Card Spades (Other 3)])
  -}
  instance Eq PlayingDeck where
    (==) (Deck []) (Deck []) = True
    (==) (EmptyDeck) (EmptyDeck) = True
    (==) (Deck (card:deck)) (Deck (cardb:deckb)) = card == cardb && (Deck deck) == (Deck deckb)
    (==) _ _ = False

  {-
    createEmptyDeck game
    PURPOSE: Create a playingCard deck with 52 cards, unshuffled.
    PRE:  true
    POST: a deck with 52 playing cards.
    SIDE EFFECTS: none
    EXAMPLES: createEmptyDeck == [AS][2S][3S][4S][5S][6S][7S][8S][9S][10S][JS][QS][KS][AC][2C][3C][4C]
    [5C][6C][7C][8C][9C][10C][JC][QC][KC][AD][2D][3D][4D][5D][6D][7D][8D][9D][10D][JD][QD][KD][AH][2H]
    [3H][4H][5H][6H][7H][8H][9H][10H][JH][QH][KH]
  -}
  createEmptyDeck :: PlayingDeck
  createEmptyDeck = (Deck [(Card Spades A) .. (Card Hearts K)])

  {-
    cardsFromDeck deck
    PURPOSE: return cards from a deck
    PRE:  true
    POST: a list with all cards from deck
    SIDE EFFECTS: none
    EXAMPLES: cardsFromDeck (Deck [(Card Spades A),(Card Spades (Other 2)),(Card Spades (Other 3))]) == [[AS],[2S],[3S]]
  -}
  cardsFromDeck :: PlayingDeck -> [PlayingCard]
  cardsFromDeck (Deck cards) = cards

  {-
    shuffleList stdgen list
    PURPOSE: Shuffle supplied list
    CREDITS: http://stackoverflow.com/questions/9877969/haskell-functions-to-randomly-order-a-list-not-working-properly-homework-begin
    PRE: true
    POST: a shuffled list
    SIDE EFFECTS: none
    EXAMPLES: shuffleList (mkStdGen 6324) [1,2,3,4,5,6] == [6,3,4,2,1,5]
              shuffleList (mkStdGen 333) [1,2,3,4,5,6] == [6,2,5,3,4,1]
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
    shuffleDeck deck
    PURPOSE: Shuffle supplied deck
    PRE: true
    POST: a shuffled deck
    SIDE EFFECTS: none
    EXAMPLES: TODO
  -}
  shuffleDeck :: StdGen -> PlayingDeck -> PlayingDeck
  shuffleDeck gen EmptyDeck = EmptyDeck
  shuffleDeck gen (Deck cards) = (Deck (shuffleList gen cards))

  {-
    sortDeck deck
    PURPOSE: sort a deck
    PRE: true
    POST: a sorted deck
    SIDE EFFECTS: none
    EXAMPLES: sortDeck (Deck [(Card Spades A),(Card Spades (Other 2)),(Card Spades (Other 3))]  == [AS][2S][3S]
  -}
  sortDeck :: PlayingDeck -> PlayingDeck
  sortDeck EmptyDeck = EmptyDeck
  sortDeck (Deck cards) = (Deck (sort cards))

  {-
    drawCardFromDeck deck
    PURPOSE: Draw one card from the top of the deck, if there's no more cards
      return InvisibleCard
    PRE: true
    POST: A card that is on the top of the deck. If there is no cards the function returns the InvisibleCard.
    SIDE EFFECTS: none
    EXAMPLES: drawCardFromDeck (Deck [(Card Diamonds A), (Card Spades (Other 10))]) == [AD]
              drawCardFromDeck EmptyDeck == []
  -}
  drawCardFromDeck :: PlayingDeck -> PlayingCard
  drawCardFromDeck EmptyDeck = InvisibleCard
  drawCardFromDeck (Deck []) = InvisibleCard
  drawCardFromDeck (Deck (card:_)) = card

  {-
    drawNumberOfCardsFromDeck n deck
    PURPOSE: draw n number of cards from deck
    PRE: true
    POST: a list of n cards drawn from the top of deck
    EXAMPLES:
      TODO
  -}
  drawNumberOfCardsFromDeck :: Int -> PlayingDeck -> [PlayingCard]
  drawNumberOfCardsFromDeck n deck = (map (\_ -> drawCardFromDeck deck) [1 .. n])

  {-
    drawAndRemoveCardFromDeck deck
    PURPOSE: Draw and remove a card from a deck
    PRE: deck has to contain at least one card
    POST: a card and the new deck inside of a tuple.
    EXAMPLES:
      TODO
  -}
  drawAndRemoveCardFromDeck :: PlayingDeck -> (PlayingCard, PlayingDeck)
  drawAndRemoveCardFromDeck deck = (drawCardFromDeck deck, removeTopCardFromDeck deck)

  {-
    removeTopCardFromDeck deck
    PURPOSE: Remove top card from deck
    PRE: true
    POST: a deck with the top card removed
    SIDE EFFECTS: none
    EXAMPLES:
      removeTopCardFromDeck (Deck [(Card Spades A),(Card Spades (Other 2)), (Card Spades (Other 3)),(Card Spades (Other 4))]) = [2S][3S][4S]
  -}
  removeTopCardFromDeck :: PlayingDeck -> PlayingDeck
  removeTopCardFromDeck EmptyDeck = EmptyDeck
  removeTopCardFromDeck (Deck (topcard:[])) = EmptyDeck
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
  testcardsFromDeck = TestCase $ assertBool "cardsFromDeck" ((cardsFromDeck testDeck) == [Card Spades A,Card Spades (Other 2),Card Spades (Other 3),
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
  testsortDeck = TestCase $ assertBool "sortDeck" ((sortDeck testDeck) == (Deck [Card Spades A,Card Spades (Other 2),Card Spades (Other 3),
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
    Card Hearts Q,Card Hearts K]))
  testremoveTopCardFromDeck = TestCase $ assertBool "removeTopCardFromDeck" (removeTopCardFromDeck (Deck [(Card Spades A),(Card Spades (Other 2)),(Card Spades (Other 3))]) == (Deck [(Card Spades (Other 2)),(Card Spades (Other 3))]))
  testdrawAndRemoveCardFromDeck = TestCase $ assertBool "drawAndRemoveCardFromDeck" (drawAndRemoveCardFromDeck (Deck [(Card Spades A),(Card Spades (Other 2)),(Card Spades (Other 3))]) == ((Card Spades A), (Deck [(Card Spades (Other 2)),(Card Spades (Other 3))])))


  testListDeck = TestList [testCreateEmptyDeck,
                            testDrawCardFromDeck,
                            testcardsFromDeck,
                            testsortDeck,
                            testremoveTopCardFromDeck,
                            testdrawAndRemoveCardFromDeck
                            ]
