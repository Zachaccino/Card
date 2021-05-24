-- | Author: Jingyuan Tu
--   Student ID: 1232404
--  
--   This module applies an iterative reduction and guessing strategy to 
--   guess the cards drawn by the opponent from a deck of playing cards
--   without jokers.
--  
--   There exists a set of cards drawn by the opponent from a deck of playing
--   cards without jokers. The goal is to correctly guess the opponent's cards
--   using the least number of guesses possible. Feedbacks are given indicating
--   if there are larger, lower, or correct cards in the answer. The initial guess 
--   eliminates a large number of ranks by guessing cards with R6 and R10 ranks. 
--   Subsequent guesses start within the remaining set of ranks, and narrow down the
--   bounds using a binary search approach. Once the bounds are found, the program
--   deduce the cards by trying each suit with the rank at bound. This process is 
--   repeated until all cards are guessed. 


module Proj2
  ( GameState
  , feedback
  , initialGuess
  , nextGuess
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


-- | Keeping track of the overall guessing progress using a state machine. 
--   
--   The state is initiated by the initialGuess, and the subsequent state 
--   transitions are computed by the nextGuess.
data GameState
  = InitialGuess { gsInitialMinRank :: Rank
                 , gsInitialMaxRank :: Rank
                 }
  | FindingMax   { gsMaxRankGuess :: RankGuess
                 , gsEstMinRank :: Rank
                 , gsHistory :: [(Card, Int)]
                 , gsLevel :: Int
                 , gsResult :: [Card]
                 }
  | FindingMin   { gsMinRankGuess :: RankGuess
                 , gsMaxRank :: Rank
                 , gsHistory :: [(Card, Int)]
                 , gsLevel :: Int
                 , gsResult :: [Card]
                 }
  | GuessingMax  { gsSuit :: Suit
                 , gsMinRank :: Rank
                 , gsMaxRank :: Rank
                 , gsResult :: [Card]
                 , gsHistory :: [(Card, Int)]
                 , gsLevel :: Int

                 }
  | GuessingMin  { gsSuit :: Suit
                 , gsMinRank :: Rank
                 , gsMaxRank :: Rank
                 , gsResult :: [Card]
                 , gsHistory :: [(Card, Int)]
                 , gsLevel :: Int
                 }
  | Done         { gsResult :: [Card] }
  deriving (Eq, Show)


-- | Keeping track of the progress of guessing the minimum or maximum rank.
data RankGuess
  = RankGuess { rgRank :: Rank
              , rgMaxRank :: Rank
              , rgMinRank :: Rank
              , rgStep :: Int
              , rgComplete :: Bool
              }
  deriving (Eq, Show)


-- | Given the number of cards in the answer that have lower ranks than
--   the lowest ranked cards in the current guess. Produce the next guess
--   that may or may not be the minimum rank. 
--   
--   This function locates the minimum rank by doing a binary search. 
-- 
--   Once the search is completed, the completion status in the next guess
--   is set to True, and subsequent guess would not change the guess outcome.
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


-- | Given the number of cards in the answer that have higher ranks than
--   the highest ranked cards in the current guess. Produce the next guess
--   that may or may not be the maximum rank. 
--   
--   This function locates the maximum rank by doing a binary search. 
-- 
--   Once the search is completed, the completion status in the next guess
--   is set to True, and subsequent guess would not change the guess outcome.
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


-- | Given the answer cards and guess cards, returns the a feedback tutple
--   containing the number of correct cards, number of cards with lower ranks,
--   number of cards share the same rank, number of cards with higher ranks,
--   and number of cards share the same suit.
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
  -- The size of the intersection of two lists. Assuming both lists
  -- are sorted ahead of time. 
  matchCount :: (Ord a, Eq a) => [a] -> [a] -> Int
  matchCount [] _  = 0
  matchCount _  [] = 0
  matchCount (x : xs) (y : ys) | x == y    = matchCount xs ys + 1
                               | x > y     = matchCount (x : xs) ys
                               | otherwise = matchCount xs (y : ys)


-- | Given the number of cards in the answer cards. Produce an initial guess 
--   and game state such that subsequent guesses can be computed.
initialGuess :: Int -> ([Card], GameState)
initialGuess size =
  let suit      = Club
      minRank   = R6
      maxRank   = R10
      guess     = take
        size
        [ Card suit minRank
        , Card suit maxRank
        , Card suit (succ minRank)
        , Card suit (pred maxRank)
        ]
      gameState = InitialGuess minRank maxRank
  in  (guess, gameState)


-- | Given the number of cards and the desired rank, make a list of
--   valid and unique cards with the same rank but with different suit.
makeCards :: Int -> Rank  -> [Card]
makeCards size rank =
  let combinations = take size (zip [Club ..] (repeat rank))
  in  map (\(s, r) -> Card s r) combinations


-- | Given the guess cards and the number of correct cards, convert
--   them into the guess histroy, where each card in the guess is associated 
--   with the number of correct cards. 
toHistory :: [Card] -> Int -> [(Card, Int)]
toHistory guess corrects = map (\c -> (c, corrects)) guess


-- | Given the number of cards, the desired suit and rank, and guess history.
--   Make a list of cards where only the card with given suit and rank can 
--   be the correct card, and all other cards must be incorrect as indicated by
--   the history.
makeFocusGuess :: Int -> Suit -> Rank -> [(Card, Int)]-> [Card]
makeFocusGuess size targetSuit rank history =
  let wrongChoices = map fst . take (size - 1) $ filter (\(Card s _, c) -> c == 0 && s /= targetSuit) history
  in Card targetSuit rank : wrongChoices



-- | Given the current guess, game state, and feedback, generate another guess and game state 
--   that is closer to guessing the right cards. 
nextGuess
  :: ([Card], GameState) -> (Int, Int, Int, Int, Int) -> ([Card], GameState)

-- | Determining the starting point for binary searching max and min rank, and preparing for guessing the max rank.
nextGuess (currentGuess, InitialGuess initMinRank initMaxRank) (_, lowerRanks, _, higherRanks, correctSuits)
  = let maxRank       = if higherRanks == 0 then initMaxRank else maxBound
        minRank       = if lowerRanks == 0 then initMinRank else minBound
        maxRankGuess  = RankGuess { rgRank     = maxRank
                                  , rgMaxRank  = maxRank
                                  , rgMinRank  = minRank
                                  , rgStep = fromEnum maxRank - fromEnum minRank
                                  , rgComplete = False
                                  }
        nextState = FindingMax maxRankGuess minRank [] 0 []
        guess     = makeCards (length currentGuess) maxRank
    in  (guess, nextState)

-- | Narrowing down the max rank by making a guess that is guided by the number of higher rank cards in the feedback.
--   Guess higher rank if the answer contains higher rank cards and vice versa.
--
--   Once max rank is found, it prepares to guess the min rank with the knowledge of the known max bound.
nextGuess (currentGuess, currentState@(FindingMax maxRankGuess estMinRank history level result)) (correctCards, _, _, higherRanks, _)
  = let nextMaxRankGuess = guessMaxRank (higherRanks - level) maxRankGuess
        nextHistory      = toHistory currentGuess correctCards ++ history
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
                wrongRanks = if maxRank /= maxBound then [(succ maxRank)..maxBound] else []
                -- Once we found the max bound, we can assume ranks above max bound are incorrect. 
                presumeHistory = map (\(s, r) -> (Card s r, 0)) $ [ (s, r) | s<-[minBound ..], r<-wrongRanks ]
            in  FindingMin minRankGuess maxRank (nextHistory ++ presumeHistory) level result
          else currentState { gsMaxRankGuess = nextMaxRankGuess, gsHistory = nextHistory }
        guess = makeCards
          (length currentGuess)
          (if rgComplete nextMaxRankGuess
            then estMinRank
            else rgRank nextMaxRankGuess
          )
    in  (guess, nextState)


-- | Narrowing down the min rank by making a guess that is guided by the number of lower rank cards in the feedback.
--   Guess lower rank if the answer contains lower rank cards and vice versa.
--
--   Once min rank is found, it prepares to guess the suit of the highest ranked card in the answer.
nextGuess (currentGuess, currentState@(FindingMin minRankGuess maxRank history level result)) (correctCards, lowerRanks, _, _, _)
  = let nextMinRankGuess = guessMinRank (lowerRanks - level) minRankGuess
        nextHistory       = toHistory currentGuess correctCards ++ history
        nextState        = if rgComplete nextMinRankGuess
          then
            let minRank = rgRank nextMinRankGuess
                wrongRanks = if minRank /= minBound then [(pred minRank)..minBound] else []
                -- Once we found the min bound, we can assume ranks below min bound are incorrect. 
                presumeHistory = map (\(s, r) -> (Card s r, 0)) $ [ (s, r) | s<-[minBound ..], r<-wrongRanks ]
                -- Start guessing the suit from Club to Diamon in order.
            in  GuessingMax Club minRank maxRank result (nextHistory ++ presumeHistory) level 
          else currentState { gsMinRankGuess = nextMinRankGuess, gsHistory = nextHistory }
        guess = case nextState of
          GuessingMax suit _ _ _ hist _ -> makeFocusGuess (length currentGuess) suit maxRank hist
          _ -> makeCards (length currentGuess) (rgRank nextMinRankGuess)
    in  (guess, nextState)

-- | Guess each suit with the max rank and keep them in the result if it is one of the answer card. 
--   Once all suits had been teseted with the max rank, do the same for min rank.
--
--   If all cards had been found during guessing,  the guessing is set to be done. 
nextGuess (currentGuess, currentState@(GuessingMax suit minRank maxRank result history level)) (correctCards, _, _, _, _)
  = let nextResult = if correctCards > 0 then Card suit maxRank : result else result
        nextHistory = toHistory currentGuess correctCards ++ history
        nextState
          | length nextResult == length currentGuess = Done nextResult
          | suit == maxBound = GuessingMin Club minRank maxRank nextResult nextHistory level
          | otherwise = currentState { gsSuit = succ suit, gsResult = nextResult, gsHistory = nextHistory }
        guess = case nextState of
          GuessingMin suit _ _ _ hist _ -> makeFocusGuess (length currentGuess) suit minRank hist
          GuessingMax suit _ _ _ hist _ -> makeFocusGuess (length currentGuess) suit maxRank hist
          Done found -> found
    in (guess, nextState)

-- | Guess each suit with the min rank and keep them in the result if it is one of the answer card. 
--   Once all suits had been teseted with the min rank, the guessing stops if all results had been found,
---  otherwise, it is back to finding the max bound but excluding the cards that we have found. 
nextGuess (currentGuess, currentState@(GuessingMin suit minRank maxRank result history level)) (correctCards, _, _, _, _)
  = let nextResult = if correctCards > 0 then Card suit minRank : result else result
        nextHistory = toHistory currentGuess correctCards ++ history
        nextState 
          | length nextResult == length currentGuess = Done nextResult
          | suit == maxBound = let maxRankGuess  = RankGuess { rgRank     = pred maxRank
                                                              , rgMaxRank  = pred maxRank
                                                              , rgMinRank  = succ minRank
                                                              , rgStep = fromEnum (pred maxRank) - fromEnum (succ minRank)
                                                              , rgComplete = False
                                                              }
                                in FindingMax maxRankGuess (succ minRank) nextHistory (level + 1) nextResult
          | otherwise = currentState { gsSuit = succ suit, gsResult = nextResult, gsHistory = nextHistory }
        guess = case nextState of
          Done found -> found
          GuessingMin suit _ _ _ hist _ -> makeFocusGuess (length currentGuess) suit minRank hist
          FindingMax maxRankGuess _ _ _ _  -> makeCards (length currentGuess) (rgRank maxRankGuess)
    in (guess, nextState)


-- | Once the guess is final, it would not change.
nextGuess (currentGuess, currentState@Done {}) _ =  (currentGuess, currentState)