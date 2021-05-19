{-# LANGUAGE BlockArguments #-}
-- Replace this comment with your opening documentation.  Leave this module declaration as is:
module Proj2
  ( feedback
  , initialGuess
  , nextGuess
  , GameState
  , testMinRankGuess
  , testMaxRankGuess
  )
where


import qualified Card
import           Card                           ( Card
                                                , Rank(Queen, R2)
                                                , Suit
                                                )
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )

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
  let maxRank = foldl (\acc card -> max acc $ Card.rank card)
                      (minBound :: Rank)
                      answer
      minRank = foldl (\acc card -> min acc $ Card.rank card)
                      (maxBound :: Rank)
                      answer
      suits = foldl (\acc card -> Set.insert (Card.suit card) acc)
                    (mempty :: Set Suit)
                    answer
      correctCards = Set.intersection (Set.fromList answer :: Set Card)
                                      (Set.fromList guess :: Set Card)
  in  (1, 1, 1, 1, 1)



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
