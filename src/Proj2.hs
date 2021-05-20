module Proj2
  ( testMinRankGuess
  , testMaxRankGuess
  , guessSuits
  , GameState
  , SuitGuess(..)
  -- ( feedback
  -- , initialGuess
  -- , nextGuess
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
  = InitialGuess
  | FindingMax
  | FindingMin
  | FindingSuit
  | Guessing
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

guessSuits :: Bool -> SuitGuess -> SuitGuess
guessSuits correct currentGuess@(SuitGuess suit found completed)
  | completed = currentGuess
  | suit == maxBound = if correct
    then currentGuess { sgFound = suit : found, sgComplete = True }
    else currentGuess { sgFound = found, sgComplete = True }
  | otherwise = if correct
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
  let initialSuit  = Club
      suitGuess    = SuitGuess { sgSuit = initialSuit, sgFound = [] }
      baseCards    = [Card initialSuit R6, Card initialSuit R10]
      miscCards    = [Card initialSuit R7, Card initialSuit R9]
      initialCards = take (size - 2) miscCards ++ baseCards
  in  (initialCards, InitialGuess suitGuess)







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
