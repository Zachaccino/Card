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
  , guessSuits
  , SuitGuess(..)
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
  = InitialGuess { gsSuitsGuess :: SuitGuess
                 }
  | GameReducing { gsMinRankGuess :: RankGuess
                 , gsMaxRankGuess :: RankGuess
                 , gsSuitsGuess :: SuitGuess
                 }
  | GameGuessing
  | GameGuessed
  deriving (Eq, Show)

data RankGuess
  = RankGuessing { rgRank :: Rank
                 , rgMaxRank :: Rank
                 , rgMinRank :: Rank
                 , rgStep :: Int
                 }
  | RankGuessed  { rgAnswer :: Rank
                 }
  deriving (Eq, Show)


guessMinRank :: Int -> RankGuess -> RankGuess
guessMinRank lowerRanks currentGuess@(RankGuessing rank maxRank minRank step) =
  let nextMaxRank = if lowerRanks == 0 then maxRank else pred rank
      nextMinRank = if lowerRanks == 0 then rank else minRank
      nextStep    = ceiling (fromIntegral step / 2)
      direction   = if lowerRanks == 0 then 1 else -1
      converged   = nextMaxRank == nextMinRank
  in  if converged
        then RankGuessed nextMaxRank
        else currentGuess
          { rgRank    = toEnum $ fromEnum rank + direction * nextStep
          , rgMaxRank = nextMaxRank
          , rgMinRank = nextMinRank
          , rgStep    = nextStep
          }
guessMinRank _ correctGuess = correctGuess


guessMaxRank :: Int -> RankGuess -> RankGuess
guessMaxRank higherRanks currentGuess@(RankGuessing rank maxRank minRank step)
  = let nextMaxRank = if higherRanks == 0 then rank else maxRank
        nextMinRank = if higherRanks == 0 then minRank else succ rank
        nextStep    = ceiling (fromIntegral step / 2)
        direction   = if higherRanks == 0 then -1 else 1
        converged   = nextMaxRank == nextMinRank
    in  if converged
          then RankGuessed nextMaxRank
          else currentGuess
            { rgRank    = toEnum $ fromEnum rank + direction * nextStep
            , rgMaxRank = nextMaxRank
            , rgMinRank = nextMinRank
            , rgStep    = nextStep
            }
guessMaxRank _ correctGuess = correctGuess


data SuitGuess
  = SuitGuessing { sgSuit :: Suit
                 , sgFound :: [Suit]
                 }
  | SuitGuessed  { sgAnswer :: [Suit]
                 }
    deriving(Eq, Show)

guessSuits :: Bool -> SuitGuess -> SuitGuess
guessSuits correct currentGuess@(SuitGuessing suit found)
  | suit == maxBound = if correct
    then SuitGuessed (suit : found)
    else SuitGuessed found
  | otherwise = if correct
    then SuitGuessing { sgSuit = succ suit, sgFound = suit : found }
    else SuitGuessing { sgSuit = succ suit, sgFound = found }
guessSuits _ correctGuess = correctGuess


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
      suitGuess    = SuitGuessing { sgSuit = initialSuit, sgFound = [] }
      baseCards    = [Card initialSuit R6, Card initialSuit R10]
      miscCards    = [Card initialSuit R7, Card initialSuit R9]
      initialCards = take (size - 2) miscCards ++ baseCards
  in  (initialCards, InitialGuess suitGuess)


nextGuess
  :: ([Card], GameState) -> (Int, Int, Int, Int, Int) -> ([Card], GameState)
nextGuess (initialGuess, InitialGuess suitGuess) (corrects, lowerRanks, correctRanks, higherRanks, correctSuits)
  = let initMaxRank  = if higherRanks == 0 then R10 else maxBound
        initMinRank  = if lowerRanks == 0 then R6 else minBound
        minRankGuess = RankGuessing
          { rgRank    = initMinRank
          , rgMaxRank = initMaxRank
          , rgMinRank = initMinRank
          , rgStep    = fromEnum initMaxRank - fromEnum initMinRank
          }
        maxRankGuess = RankGuessing
          { rgRank    = initMaxRank
          , rgMaxRank = initMaxRank
          , rgMinRank = initMinRank
          , rgStep    = fromEnum initMaxRank - fromEnum initMinRank
          }
        nextSuitGuess = guessSuits (correctSuits > 0) suitGuess
        nextGameState = GameReducing { gsMinRankGuess = minRankGuess
                                     , gsMaxRankGuess = maxRankGuess
                                     , gsSuitsGuess   = nextSuitGuess
                                     }
        baseGuess =
            [ Card (sgSuit nextSuitGuess) initMinRank
            , Card (sgSuit nextSuitGuess) initMaxRank
            ]
        miscGuess =
            [ Card (sgSuit nextSuitGuess) (succ initMinRank)
            , Card (sgSuit nextSuitGuess) (pred initMaxRank)
            ]
        nextGuess = take (length initialGuess - 2) miscGuess ++ baseGuess
    in  (nextGuess, nextGameState)


-- Unit Tests
testMinRankGuess :: Rank -> (Int, [Rank], Rank)
testMinRankGuess lowestAnsRank =
  let initialGuess = RankGuessing { rgRank    = toEnum 4
                                  , rgMaxRank = toEnum 8
                                  , rgMinRank = toEnum 4
                                  , rgStep    = 4
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
