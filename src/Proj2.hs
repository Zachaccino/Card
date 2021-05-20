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
import qualified Data.Maybe                    as Maybe

data GameState
  = GameReducing { gsMinRankGuess :: RankGuess
                 , gsMaxRankGuess :: RankGuess
                 , gsSuitsGuess :: SuitGuess
                 }
  | GameGuessing
  | GameGuessed


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
  = SuitGuess { sgMinSuit :: Suit
              , sgMaxSuit :: Suit
              , sgProcessedLeft :: Bool
              , sgCorrects :: Maybe Int
              , sgFound :: Int
              }

stackPush :: a -> [a] -> [a]
stackPush x stack = x : stack

stackPop :: [a] -> (Maybe a, [a])
stackPop (x : xs) = (Just x, xs)
stackPop []       = (Nothing, [])

guessSuits :: [SuitGuess] -> ([SuitGuess], [Suit])
guessSuits [] = ([], [])
guessSuits stack =
  let (Just currentGuess, stack) = stackPop stack
  in
    if (sgMinSuit currentGuess == sgMaxSuit currentGuess)
       || (Maybe.fromJust (sgCorrects currentGuess) == sgFound currentGuess)
    then
      let (parentGuess, stack) = stackPop stack
      in  if Maybe.isNothing parentGuess
            then (stack, [])
            else
              let parentGuess' = (Maybe.fromJust parentGuess)
                    { sgFound = sgFound (Maybe.fromJust parentGuess) + 1
                    }
                  stack'               = stackPush parentGuess' stack
                  (nextStack, results) = guessSuits stack'
              in  ( nextStack
                  , results
                    ++ [ sgMinSuit currentGuess
                       | Maybe.fromJust (sgCorrects currentGuess) /= 0
                       ]
                  )
    else
      if fromEnum (sgMaxSuit currentGuess)
         -  ceiling (fromIntegral (fromEnum (sgMaxSuit currentGuess)) / 2)
         == sgFound currentGuess
      then
        (stack, [])
      else
        if not (sgProcessedLeft currentGuess)
          then
            let nextGuess = currentGuess
                  { sgMaxSuit       = toEnum
                                      . ceiling
                                      $ fromIntegral
                                          (fromEnum (sgMaxSuit currentGuess))
                                      / 2 :: Suit
                  , sgProcessedLeft = False
                  , sgCorrects      = Nothing
                  , sgFound         = 0
                  }
                currentGuess' = currentGuess { sgProcessedLeft = True }
                stack' = stackPush nextGuess $ stackPush currentGuess' stack
            in  (stack', [])
          else
            let
              nextGuess = currentGuess
                { sgMinSuit       =
                  toEnum
                  $ ceiling
                      (fromIntegral (fromEnum (sgMaxSuit currentGuess)) / 2)
                  + 1 :: Suit
                , sgProcessedLeft = False
                , sgCorrects      = Nothing
                , sgFound         = 0
                }
              stack' = stackPush nextGuess $ stackPush currentGuess stack
            in
              (stack', [])







  -- | processedLeft && processedRight
  -- -- TODO: Which switch to turn.
  -- = guessSuits (Maybe.fromJust parentGuess) { sgFound = found }
  -- | minSuit == maxSuit
  -- = if Maybe.isNothing parentGuess
  --   then if Maybe.fromJust corrects == 0
  --     then SuitGuessed []
  --     else SuitGuessed [minSuit]
  --   else
  --     if Maybe.fromJust (sgCorrects (Maybe.fromJust parentGuess))
  --        == length found
  --     then
  --       guessSuits (Maybe.fromJust parentGuess) { sgFound          = found
  --                                               , sgProcessedLeft  = True
  --                                               , sgProcessedRight = True
  --                                               }
  --     else
  --       SuitGuessing
  --         { sgMinSuit        = toEnum (fromEnum maxSuit + 1) :: Suit
  --         , sgMaxSuit        = sgMaxSuit $ Maybe.fromJust parentGuess
  --         , sgCorrects       = Nothing
  --         , sgParentGuess = Just (Maybe.fromJust parentGuess) { sgProcessedLeft = True
  --                                                             }
  --         , sgFound          = found
  --         , sgProcessedLeft  = False
  --         , sgProcessedRight = False
  --         }
  -- | processedLeft
  -- = SuitGuessing { sgMinSuit        = toEnum (fromEnum maxSuit + 1) :: Suit
  --                , sgMaxSuit        = sgMaxSuit $ Maybe.fromJust parentGuess
  --                , sgCorrects       = Nothing
  --                , sgParentGuess    = Just currentGuess
  --                , sgFound          = []
  --                , sgProcessedLeft  = True
  --                , sgProcessedRight = False
  --                }
  -- | otherwise
  -- = SuitGuessing
  --   { sgMinSuit        = minSuit
  --   , sgMaxSuit = toEnum . ceiling $ fromIntegral (fromEnum maxSuit) / 2 :: Suit
  --   , sgCorrects       = Nothing
  --   , sgParentGuess    = Just currentGuess
  --   , sgFound          = []
  --   , sgProcessedLeft  = False
  --   , sgProcessedRight = False
  --   }





-- SuitGuessing { sgMinSuit     = toEnum (fromEnum maxSuit + 1) :: Suit
--                         , sgMaxSuit     = sgMaxSuit $ Maybe.fromJust parentGuess
--                         , sgCorrects    = Nothing
--                         , sgParentGuess = parentGuess
--                         , sgFound       = []
--                         }

  --   if Maybe.fromJust corrects > 0
  --   then SuitGuessed [minSuit]
  --   case parentGuess of
  --   Nothing     -> SuitGuessed found
  --   Just parent -> if
  --     SuitGuessing
  --     { sgMinSuit     = toEnum (fromEnum maxSuit + 1) :: Suit
  --     , sgMaxSuit     = sgMaxSuit parent
  --     , sgCorrects    = Nothing
  --     , sgParentGuess = Just parent
  --     , sgFound       = []
  --     }
  -- | otherwise
  -- = SuitGuessing
  --   { sgMinSuit     = minSuit
  --   , sgMaxSuit = toEnum . ceiling $ fromIntegral (fromEnum maxSuit) / 2 :: Suit
  --   , sgCorrects    = Nothing
  --   , sgParentGuess = Just currentGuess
  --   , sgFound       = []
  --   }


-- guessSuits correctGuess       = correctGuess


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
