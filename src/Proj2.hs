module Proj2
  ( GameState
  , SuitGuess(..)
  , feedback
  , initialGuess
  , nextGuess
  , testMinRankGuess
  , testMaxRankGuess
  , guessSuits
  , makeCards
  , testGame
  )
where


import qualified Card
import           Card                           ( Card(Card)
                                                , Rank
                                                  ( Queen
                                                  , R2
                                                  , R3
                                                  , R4
                                                  , R6
                                                  , R7
                                                  , R9
                                                  , R10
                                                  , Ace
                                                  )
                                                , Suit
                                                  ( Spade
                                                  , Heart
                                                  , Diamond
                                                  , Club
                                                  )
                                                )
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import qualified Data.List                     as List
import qualified Data.Maybe                    as Maybe


data GameState
  = InitialGuess { gsInitialMinRank :: Rank
                 , gsInitialMaxRank :: Rank
                 , gsSuitGuess :: SuitGuess
                 }
  | FindingMax   { gsMaxRankGuess :: RankGuess
                 , gsEstMinRank :: Rank
                 , gsSuitGuess :: SuitGuess
                 }
  | FindingMin   { gsMinRankGuess :: RankGuess
                 , gsMaxRank :: Rank
                 , gsSuitGuess :: SuitGuess
                 }
  | GuessingMax  { gsMinRank :: Rank
                 , gsMaxRank :: Rank
                 , gsSuitGuess :: SuitGuess
                 , gsResult :: [Card]
                 }
  | GuessingMin  { gsMinRank :: Rank
                 , gsMaxRank :: Rank
                 , gsSuitGuess :: SuitGuess
                 , gsResult :: [Card]
                 }
  | Done
  deriving (Eq, Show)


data RankGuess
  = RankGuess { rgRank :: Rank
              , rgMaxRank :: Rank
              , rgMinRank :: Rank
              , rgStep :: Int
              , rgComplete :: Bool
              }
  deriving (Eq, Show)

data SuitGuess
  = SuitGuess { sgSuit :: Suit
              , sgFound :: [Suit]
              , sgComplete :: Bool
              }
    deriving(Eq, Show)


guessMinRank :: Int -> RankGuess -> RankGuess
guessMinRank lowerRanks currentGuess@(RankGuess rank maxRank minRank step completed)
  | completed
  = currentGuess
  | otherwise
  = let nextMaxRank = if lowerRanks == 0 then maxRank else pred rank
        nextMinRank = if lowerRanks == 0 then rank else minRank
        nextStep    = ceiling (fromIntegral step / 2)
        direction   = if lowerRanks == 0 then 1 else -1
        converged   = nextMaxRank == nextMinRank
    in  if converged
          then currentGuess { rgRank     = nextMaxRank
                            , rgMaxRank  = nextMaxRank
                            , rgMinRank  = nextMinRank
                            , rgStep     = nextStep
                            , rgComplete = True
                            }
          else currentGuess
            { rgRank    = toEnum $ fromEnum rank + direction * nextStep
            , rgMaxRank = nextMaxRank
            , rgMinRank = nextMinRank
            , rgStep    = nextStep
            }


guessMaxRank :: Int -> RankGuess -> RankGuess
guessMaxRank higherRanks currentGuess@(RankGuess rank maxRank minRank step completed)
  | completed
  = currentGuess
  | otherwise
  = let nextMaxRank = if higherRanks == 0 then rank else maxRank
        nextMinRank = if higherRanks == 0 then minRank else succ rank
        nextStep    = ceiling (fromIntegral step / 2)
        direction   = if higherRanks == 0 then -1 else 1
        converged   = nextMaxRank == nextMinRank
    in  if converged
          then currentGuess { rgRank     = nextMaxRank
                            , rgMaxRank  = nextMaxRank
                            , rgMinRank  = nextMinRank
                            , rgStep     = nextStep
                            , rgComplete = True
                            }
          else currentGuess
            { rgRank    = toEnum $ fromEnum rank + direction * nextStep
            , rgMaxRank = nextMaxRank
            , rgMinRank = nextMinRank
            , rgStep    = nextStep
            }


guessSuits :: Int -> SuitGuess -> SuitGuess
guessSuits correct currentGuess@(SuitGuess suit found completed)
  | completed = currentGuess
  | suit == maxBound = if correct > 0
    then currentGuess { sgFound = suit : found, sgComplete = True }
    else currentGuess { sgFound = found, sgComplete = True }
  | otherwise = if correct > 0
    then currentGuess { sgSuit = succ suit, sgFound = suit : found }
    else currentGuess { sgSuit = succ suit, sgFound = found }


feedback :: [Card] -> [Card] -> (Int, Int, Int, Int, Int)
feedback answer guess =
  let
    maxGuessRank =
      foldl (\acc card -> max acc $ Card.rank card) (minBound :: Rank) guess
    minGuessRank =
      foldl (\acc card -> min acc $ Card.rank card) (maxBound :: Rank) guess
    answerSuits =
      List.sort $ foldl (\acc card -> Card.suit card : acc) [] answer
    guessSuits = List.sort $ foldl (\acc card -> Card.suit card : acc) [] guess
    answerRanks =
      List.sort $ foldl (\acc card -> Card.rank card : acc) [] answer
    guessRanks = List.sort $ foldl (\acc card -> Card.rank card : acc) [] guess
    correctCount = Set.size $ Set.intersection
      (Set.fromList answer :: Set Card)
      (Set.fromList guess :: Set Card)
    lowerCount = foldl
      (\acc card -> if Card.rank card < minGuessRank then acc + 1 else acc)
      0
      answer
    higherCount = foldl
      (\acc card -> if Card.rank card > maxGuessRank then acc + 1 else acc)
      0
      answer
    matchingSuitCount = matchCount answerSuits guessSuits
    matchingRankCount = matchCount answerRanks guessRanks
  in
    ( correctCount
    , lowerCount
    , matchingRankCount
    , higherCount
    , matchingSuitCount
    )
 where
  matchCount :: (Ord a, Eq a) => [a] -> [a] -> Int
  matchCount [] _  = 0
  matchCount _  [] = 0
  matchCount (x : xs) (y : ys) | x == y    = matchCount xs ys + 1
                               | x > y     = matchCount (x : xs) ys
                               | otherwise = matchCount xs (y : ys)



initialGuess :: Int -> ([Card], GameState)
initialGuess size =
  let suitGuess = SuitGuess Club [] False
      suit      = sgSuit suitGuess
      minRank   = R6
      maxRank   = R10
      guess     = take
        size
        [ Card suit minRank
        , Card suit maxRank
        , Card suit (succ minRank)
        , Card suit (pred maxRank)
        ]
      gameState = InitialGuess minRank maxRank suitGuess
  in  (guess, gameState)


makeCards :: Int -> Rank -> Suit -> [Card]
makeCards size rank suit =
  let combinations = take size (zip [suit ..] (repeat rank))
  in  map (\(s, r) -> Card s r) combinations

-- (correctCards, lowerRanks, correctRanks, higherRanks, correctSuits)
nextGuess
  :: ([Card], GameState) -> (Int, Int, Int, Int, Int) -> ([Card], GameState)
nextGuess (currentGuess, InitialGuess initMaxRank initMinRank suitGuess) (_, lowerRanks, _, higherRanks, correctSuits)
  = let maxRank       = if higherRanks == 0 then initMaxRank else maxBound
        minRank       = if lowerRanks == 0 then initMinRank else minBound
        nextSuitGuess = guessSuits correctSuits suitGuess
        suit          = sgSuit nextSuitGuess
        maxRankGuess  = RankGuess { rgRank     = maxRank
                                  , rgMaxRank  = maxRank
                                  , rgMinRank  = minRank
                                  , rgStep = fromEnum maxRank - fromEnum minRank
                                  , rgComplete = False
                                  }
        nextState = FindingMax maxRankGuess minRank nextSuitGuess
        guess     = makeCards (length currentGuess) maxRank suit
    in  (guess, nextState)

nextGuess (currentGuess, currentState@(FindingMax maxRankGuess estMinRank suitGuess)) (_, _, _, higherRanks, correctSuits)
  = let nextMaxRankGuess = guessMaxRank higherRanks maxRankGuess
        nextSuitGuess    = guessSuits correctSuits suitGuess
        suit             = sgSuit nextSuitGuess
        nextState        = if rgComplete nextMaxRankGuess
          then
            let maxRank      = rgRank nextMaxRankGuess
                minRankGuess = RankGuess
                  { rgRank     = estMinRank
                  , rgMaxRank  = maxRank
                  , rgMinRank  = estMinRank
                  , rgStep     = fromEnum maxRank - fromEnum estMinRank
                  , rgComplete = False
                  }
            in  FindingMin minRankGuess maxRank nextSuitGuess
          else currentState { gsMaxRankGuess = nextMaxRankGuess }
        guess = makeCards
          (length currentGuess)
          (if rgComplete nextMaxRankGuess
            then estMinRank
            else rgRank nextMaxRankGuess
          )
          suit
    in  (guess, nextState)

nextGuess (currentGuess, currentState@(FindingMin minRankGuess maxRank suitGuess)) (_, lowerRanks, _, _, correctSuits)
  = let nextMinRankGuess = guessMinRank lowerRanks minRankGuess
        nextSuitGuess    = guessSuits correctSuits suitGuess
        suit             = sgSuit nextSuitGuess
        nextState        = if rgComplete nextMinRankGuess
          then
            let minRank = rgRank nextMinRankGuess
            in  GuessingMax minRank maxRank nextSuitGuess []
          else currentState { gsMinRankGuess = nextMinRankGuess }
        guess = makeCards
          (length currentGuess)
          (if rgComplete nextMinRankGuess
            then
            else rgRank nextMinRankGuess
          )
          suit
    in  (guess, nextState)







-- Unit Tests
testMinRankGuess :: Rank -> (Int, [Rank], Rank)
testMinRankGuess lowestAnsRank =
  let initialGuess = RankGuess { rgRank     = toEnum 0
                               , rgMaxRank  = toEnum 12
                               , rgMinRank  = toEnum 0
                               , rgStep     = 12
                               , rgComplete = False
                               }
  in  loop initialGuess lowestAnsRank 0 [] 10
 where
  loop :: RankGuess -> Rank -> Int -> [Rank] -> Int -> (Int, [Rank], Rank)
  loop prevGuess lowestAnsRank guesses history cutoff =
    let feedback  = if lowestAnsRank < rgRank prevGuess then 1 else 0
        nextGuess = guessMinRank feedback prevGuess
    in  if guesses >= cutoff
          then (guesses, history, R2)
          else case nextGuess of
            RankGuess { rgComplete = False } -> loop
              nextGuess
              lowestAnsRank
              (guesses + 1)
              (history ++ [rgRank nextGuess])
              cutoff
            RankGuess { rgRank = rank, rgComplete = True } ->
              (guesses + 1, history ++ [rank], rank)


testMaxRankGuess :: Rank -> (Int, [Rank], Rank)
testMaxRankGuess highestAnsRank =
  let initialGuess = RankGuess { rgRank     = toEnum 12
                               , rgMaxRank  = toEnum 12
                               , rgMinRank  = toEnum 0
                               , rgStep     = 12
                               , rgComplete = False
                               }
  in  loop initialGuess highestAnsRank 0 [] 10
 where
  loop :: RankGuess -> Rank -> Int -> [Rank] -> Int -> (Int, [Rank], Rank)
  loop prevGuess highestAnsRank guesses history cutoff =
    let feedback  = if highestAnsRank > rgRank prevGuess then 1 else 0
        nextGuess = guessMaxRank feedback prevGuess
    in  if guesses >= cutoff
          then (guesses, history, R2)
          else case nextGuess of
            RankGuess { rgComplete = False } -> loop
              nextGuess
              highestAnsRank
              (guesses + 1)
              (history ++ [rgRank nextGuess])
              cutoff
            RankGuess { rgRank = rank, rgComplete = True } ->
              (guesses + 1, history ++ [rank], rank)


testGame :: [Card] -> [([Card], GameState)]
testGame answer =
  let initialGameState = initialGuess $ length answer
      initialFeedback  = feedback answer (fst initialGameState)
      initialHistory   = [initialGameState]
  in  loop initialGameState initialHistory 0 15
 where
  loop
    :: ([Card], GameState)
    -> [([Card], GameState)]
    -> Int
    -> Int
    -> [([Card], GameState)]
  loop gs history c cutoff =
    let currentFeedback = feedback answer (fst gs)
        nextGameState   = nextGuess gs currentFeedback
        nextHistory     = history ++ [nextGameState]
    in  if c > cutoff
          then nextHistory
          else loop nextGameState nextHistory (c + 1) cutoff
