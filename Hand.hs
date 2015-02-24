module Hand where

  import Data.List
  import Card
  import Game

  {- CLASSES -}

  class HandValue a where
    sumOfHand :: a -> Int
    numberOfCards :: a -> Int
    maximumNumberOfCards :: a -> Int

  {- DATA -}

  data PlayingHand = Hand [PlayingCard] Game

  data CardCount = MinimumCards Int
                 | MaximumCards Int
                 | NoLimit


  {- INSTANCES -}

  instance Show PlayingHand where
    show (Hand [] _) = []
    show (Hand (card:xs) game) = show(card) ++ " " ++ show(Hand xs game)

  instance HandValue PlayingHand where
    sumOfHand (Hand [] _) = 0
    sumOfHand (Hand (card:rest) game) = (valueOf card) + (sumOfHand (Hand rest game))

    numberOfCards (Hand cards BJ) = undefined
    maximumNumberOfCards (Hand cards BJ) = NoLimit

  -- TODO:
  -- instance Ord PlayingHand where

  {- FUNCTIONS -}

  {-
    TODO
    PURPOSE: Return the card at the supplied position
    HINT: !!
  -}
  cardAtPosition :: PlayingHand -> Int -> PlayingCard
  cardAtPosition (Hand cards _) position = cards !! position

  -- snyggast så ? fåååår jag puuusha :D:D:D:D:D:D:D:D:D:D:DD:D:D:D:D:D:D
  --
  -- NAJS, yep! cleaaant! heheheh jaaaaaa, gärna med vår text :D ;;DD 8=====================D~<------ 0:heheheheh hahahaha PUSHA NU TOM! WOWOWOWO VÅÅÅÅGAR INTE m
  {-
    TODO
    PURPOSE: Remove the card and return the new hand.
    HERE*S JOHNNYYUYYYYYYYYY YY Y YY Y YY YY
  -}
  -- funkar det att skriva så ? eller borde jag tänka annorluna?
  -- vet inte, ska kolla det funkar! Eller, det borde fungera. Fast neeej, du får ut en lista då! sätt in hela filter-funktionen
  -- i en hand så ska det fungera (Hand (filter shit ..) _)
  -- Inte för att vara en moodkiller, men jag pushade upp en ny innan, du borde synca. oj vad du spårar
  -- on fiiiiiiiiireeeeee idag, wanna touch you feeel the heeeat
  -- love me like you do, love love love me like you dooooooo
  -- öppna kommandotolken och skriv: git reset --hard HEAD
  -- jag copy pastar. stod att min var senaste...
  -- VA? .............   HEAD is now at 728a9e3 cardAtPostiton och viktigt skitsnackÅHHH,
  -- körde du en sync först? NelEleJr fan.!!NEJ ENEJNEJENJEN
  -- HAHHAHAHAA sluta. nu har jag och det fungerade?
  -- jävla firepad, jag copy-pastar, unshare filen först och stäng filen okok
  -- kan loada utan declarationen
  -- removeCardAtPosition :: PlayingHand -> Int -> PlayingHand
  -- filter ska vara på positionen där listan med kort är i Hand-constructorn
  -- måste vi inte ha !! någonstans ? ooooh, jooo, jag tänkte på en lista med siffror juuu!
  -- filter "måste" ha en anonym funktion som pattern-matchar kortet. eller fan. åhh, två sek
  -- låter inte som smidigaste sättet kanske. tror du man kan köra !! istället för /=? blir också fel ju..
  -- mm man vill ju ha handen utan kortet
  -- https://wiki.haskell.org/How_to_work_on_lists
  -- kolla på delete nth element, tydligen ska det vara trickyyyyy
  -- splitAt 3 [1,2,3,4,5,6] == [[1,2,3], [4,5,6]]
  --                                  ^ säg att vi vill ta bort den jäveln
  --                                    då borde vi kunna köra last på den första listan vi får tillbaka. shiet. kanske inte.
  -- blev kompliceeeerat
  -- ta nästa funktion, vi spånar imorgon! men tror du vi kan använda delete ?
  -- har ingen funktion delete, haskell verkar ha inbyggd delete ?  OOO, import Data.List sen delete. JAAAAAA
  -- så, delete position cards , så lätt? ditt jävla mastermind. så lätt. testar
  -- okej, jag tar alltid ut segern i förskott... FAST!
  -- delete tar bort ett element med ett visst värde, men vi har cardAtPosition, så om kör "delete (cardAtPosition position) cards" så borde det funka
  -- DET FUNGERARRRRRRRR, inte för mig, var är mitt fel?
  -- hmm, du returnerar en lista med PlayingCards, inte en hand.
  -- tänk på funktionen du skrev innan, typ samma grej.
  -- Hand ... game aaaaah fortfarande mkt skit som fuckar
  -- men du har ju inte ändrat än hahah! jag hjälpte dig lite
  -- börja med att sätta parenteser runt delete-shit
  -- och en constructor kanskeeee *hint* Hand
  -- hehehehe hahahah, vad sägs om att ersätta cards med (delete ... ...)
  -- delete ger dig en lista med kort tillbaka, och vi vill returnera en hand med den listan.
  -- EN HAND.
  -- kanske sååååå, forftande något feeel
  -- tvååå sekhahahahahahahahahah
  -- fortfarande fel för mig dock.. Funkar det för dig?
  -- jag veeet, leta efter frågetecknenaaa
  -- ehhehrsätt, i have no idea what iam doin
  -- sen borde vi kunna köra, yeap, och något mer
  -- så = splitAt position cards
  -- Kan fortfarande inte matcha rätt Hmmmmm, kan ta ett tag att hitta dem
  -- förlåt, ska vara tydligare. AAAAAAHA myyys koden
  -- sexigtD ÖvRärree
  --
  -- DÄR SATT DEN !
  -- WOOOOOOOOOO AAWWWMMAGDH
  -- DAAAAAYUUUUUUUUUUUM OH MY DAUYUM
  -- MAAAPAUSHAF AAAKA MAFFAKKAKAAAAAAAAA THIS IS NOTT AAA GAMEEEEE AHHRHRHRH
  -- PUSHA TOM, PUSHA PUSHA PUSHA PUSHA OUSGA JAKSFJKLASJDKASJD JAG SKA PUSHA SÅ JÄVLA HÅRT
  -- JAG BLIR SÅÅÅÅÅ STOLTTTTTTTTTTTTTTT
  removeCardAtPosition :: PlayingHand -> Int -> PlayingHand
  removeCardAtPosition hand@(Hand cards game) position  = (Hand (delete (cardAtPosition hand position) cards) game)
