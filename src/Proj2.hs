-- Replace this comment with your opening documentation.  Leave this module declaration as is:
module Proj2
  ( feedback
  , initialGuess
  , nextGuess
  , GameState
  )
where

import           Card                           ( Card
                                                , Rank(Queen)
                                                )

data GameState
  = LocatingRank | Done



data RankGuesser
  = RankGuessing { rgGuess :: Int -> RankGuesser -> RankGuesser
                 , rgRank :: Rank
                 , rgMaxRank :: Rank
                 , rgMinRank :: Rank
                 , rgStep :: Int
                 }
  | RankGuessed  { rgAnswer :: Rank
                 }



maxRankGuesser :: Rank -> Rank -> Rank -> Int -> RankGuesser
maxRankGuesser = RankGuessing guess
 where
  guess :: Int -> RankGuesser -> RankGuesser
  guess lowerRanks prevGuess@(RankGuessing _ rank maxRank minRank step) =
    let nextMaxRank = if lowerRanks == 0 then rank else maxRank
        nextMinRank = if lowerRanks == 0 then minRank else succ rank
        nextStep    = if lowerRanks == 0
          then floor (fromIntegral step / 2)
          else ceiling (fromIntegral step / 2)
        direction = if lowerRanks == 0 then -1 else 1
        converged = nextMaxRank == nextMinRank
    in  if converged
          then RankGuessed nextMaxRank
          else prevGuess
            { rgRank    = toEnum $ fromEnum rank + direction * nextStep
            , rgMaxRank = nextMaxRank
            , rgMinRank = nextMinRank
            , rgStep    = nextStep
            }






-- -- | guessing the maximum rank that the answer has
-- maxRankGuesser :: RankGuesser
-- maxRankGuesser =
--   let nextRankGuess =


-- -- TODO: Get this done first.
-- nextRankGuess :: Rank -> Bool -> Int -> (Rank, Maybe Int)
-- nextRankGuess guess lower absOffset =
--   let rankAdjustment = floor (fromIntegral absOffset / 2)
--       nextRank      = if lower
--         then toEnum $ fromEnum guess + rankAdjustment :: Rank
--         else toEnum $ fromEnum guess - rankAdjustment :: Rank
--       nextAbsOffset = if .....
--   in (nextRank, nextAbsOffset)






feedback :: [Card] -> [Card] -> (Int, Int, Int, Int, Int)
feedback answer guess = (1, 1, 1, 1, 1)

initialGuess :: Int -> ([Card], GameState)
initialGuess size = ([], Done)

nextGuess
  :: ([Card], GameState) -> (Int, Int, Int, Int, Int) -> ([Card], GameState)
nextGuess a b = a


