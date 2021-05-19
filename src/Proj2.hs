{-# LANGUAGE BlockArguments #-}
-- Replace this comment with your opening documentation.  Leave this module declaration as is:
module Proj2
  ( feedback
  , initialGuess
  , nextGuess
  , GameState
  , testMinRankGuess
  , testMaxRankGuess
  , testFeedback
  , testFeedbackRawOutput
  )
where


import qualified Card
import           Card                           ( Card(Card)
                                                , Rank(Queen, R2, R3, R4, Ace)
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

data GameState
  = GameReducing { gsMinRankGuess :: RankGuess
                 , gsMaxRankGuess :: RankGuess
                 }
  | GameGuessing
  | GameGuessed


-- Guessing the Answer Max or Min Rank
data RankGuess
  = RankGuessing { rgRank :: Rank
                 , rgMaxRank :: Rank
                 , rgMinRank :: Rank
                 , rgStep :: Int
                 }
  | RankGuessed  { rgAnswer :: Rank
                 }
  deriving (Eq, Ord, Show)


guessMinRank :: Int -> RankGuess -> RankGuess
guessMinRank lowerRanks prevGuess@(RankGuessing rank maxRank minRank step) =
  let nextMaxRank = if lowerRanks == 0 then maxRank else pred rank
      nextMinRank = if lowerRanks == 0 then rank else minRank
      nextStep    = ceiling (fromIntegral step / 2)
      direction   = if lowerRanks == 0 then 1 else -1
      converged   = nextMaxRank == nextMinRank
  in  if converged
        then RankGuessed nextMaxRank
        else prevGuess { rgRank = toEnum $ fromEnum rank + direction * nextStep
                       , rgMaxRank = nextMaxRank
                       , rgMinRank = nextMinRank
                       , rgStep    = nextStep
                       }
guessMinRank _ correctGuess = correctGuess


guessMaxRank :: Int -> RankGuess -> RankGuess
guessMaxRank higherRanks prevGuess@(RankGuessing rank maxRank minRank step) =
  let nextMaxRank = if higherRanks == 0 then rank else maxRank
      nextMinRank = if higherRanks == 0 then minRank else succ rank
      nextStep    = ceiling (fromIntegral step / 2)
      direction   = if higherRanks == 0 then -1 else 1
      converged   = nextMaxRank == nextMinRank
  in  if converged
        then RankGuessed nextMaxRank
        else prevGuess { rgRank = toEnum $ fromEnum rank + direction * nextStep
                       , rgMaxRank = nextMaxRank
                       , rgMinRank = nextMinRank
                       , rgStep    = nextStep
                       }
guessMaxRank _ correctGuess = correctGuess


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
initialGuess size = ([], GameGuessed)

nextGuess
  :: ([Card], GameState) -> (Int, Int, Int, Int, Int) -> ([Card], GameState)
nextGuess a b = a



-- Unit Tests
testMinRankGuess :: Rank -> (Int, [Rank], Rank)
testMinRankGuess lowestAnsRank =
  let initialGuess = RankGuessing { rgRank    = toEnum 0
                                  , rgMaxRank = toEnum 12
                                  , rgMinRank = toEnum 0
                                  , rgStep    = 12
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
            RankGuessing{} -> loop nextGuess
                                   lowestAnsRank
                                   (guesses + 1)
                                   (history ++ [rgRank nextGuess])
                                   cutoff
            RankGuessed rank -> (guesses + 1, history ++ [rank], rank)


testMaxRankGuess :: Rank -> (Int, [Rank], Rank)
testMaxRankGuess highestAnsRank =
  let initialGuess = RankGuessing { rgRank    = toEnum 12
                                  , rgMaxRank = toEnum 12
                                  , rgMinRank = toEnum 0
                                  , rgStep    = 12
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
            RankGuessing{} -> loop nextGuess
                                   highestAnsRank
                                   (guesses + 1)
                                   (history ++ [rgRank nextGuess])
                                   cutoff
            RankGuessed rank -> (guesses + 1, history ++ [rank], rank)

testFeedbackRawOutput :: [(Int, Int, Int, Int, Int)]
testFeedbackRawOutput =
  [ feedback [Card Club R3, Card Heart R4]    [Card Heart R4, Card Club R3]
  , feedback [Card Club R3, Card Heart R4]    [Card Club R3, Card Heart R3]
  , feedback [Card Diamond R3, Card Spade R3] [Card Club R3, Card Heart R3]
  , feedback [Card Club R3, Card Heart R4]    [Card Heart R2, Card Heart R3]
  , feedback [Card Club Ace, Card Club R2]    [Card Club R3, Card Heart R4]
  ]

testFeedback :: Bool
testFeedback =
  testFeedbackRawOutput
    == [ (2, 0, 2, 0, 2)
       , (1, 0, 1, 1, 2)
       , (0, 0, 2, 0, 0)
       , (0, 0, 1, 1, 1)
       , (0, 1, 0, 1, 1)
       ]
