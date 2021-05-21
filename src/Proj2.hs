module Proj2
  ( GameState
  , feedback
  , initialGuess
  , nextGuess
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


data RankGuess
  = RankGuess { rgRank :: Rank
              , rgMaxRank :: Rank
              , rgMinRank :: Rank
              , rgStep :: Int
              , rgComplete :: Bool
              }
  deriving (Eq, Show)


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

makeCards :: Int -> Rank  -> [Card]
makeCards size rank =
  let combinations = take size (zip [Club ..] (repeat rank))
  in  map (\(s, r) -> Card s r) combinations

toHistory :: [Card] -> Int -> [(Card, Int)]
toHistory guess corrects = map (\c -> (c, corrects)) guess

makeFocusGuess :: Int -> Suit -> Rank -> [(Card, Int)]-> [Card]
makeFocusGuess size targetSuit rank history =
  let wrongChoices = map fst . take (size - 1) $ filter (\(Card s _, c) -> c == 0 && s /= targetSuit) history
  in Card targetSuit rank : wrongChoices



-- (correctCards, lowerRanks, correctRanks, higherRanks, correctSuits)
nextGuess
  :: ([Card], GameState) -> (Int, Int, Int, Int, Int) -> ([Card], GameState)
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

nextGuess (currentGuess, currentState@(FindingMax maxRankGuess estMinRank history level result)) (correctCards, _, _, higherRanks, _)
  = let nextMaxRankGuess = guessMaxRank higherRanks maxRankGuess
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

nextGuess (currentGuess, currentState@(FindingMin minRankGuess maxRank history level result)) (correctCards, lowerRanks, _, _, _)
  = let nextMinRankGuess = guessMinRank lowerRanks minRankGuess
        nextHistory       = toHistory currentGuess correctCards ++ history
        nextState        = if rgComplete nextMinRankGuess
          then
            let minRank = rgRank nextMinRankGuess
                wrongRanks = if minRank /= minBound then [(pred minRank)..minBound] else []
                presumeHistory = map (\(s, r) -> (Card s r, 0)) $ [ (s, r) | s<-[minBound ..], r<-wrongRanks ]
            in  GuessingMax Club minRank maxRank result (nextHistory ++ presumeHistory) level 
          else currentState { gsMinRankGuess = nextMinRankGuess, gsHistory = nextHistory }
        guess = case nextState of
          GuessingMax suit _ _ _ hist _ -> makeFocusGuess (length currentGuess) suit maxRank hist
          _ -> makeCards (length currentGuess) (rgRank nextMinRankGuess)
    in  (guess, nextState)

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
                                in FindingMax maxRankGuess minRank nextHistory level nextResult
          | otherwise = currentState { gsSuit = succ suit, gsResult = nextResult, gsHistory = nextHistory }
        guess = case nextState of
          Done found -> found
          GuessingMin suit _ _ _ hist _ -> makeFocusGuess (length currentGuess) suit minRank hist
    in (guess, nextState)

nextGuess (currentGuess, currentState@Done {}) _ =  (currentGuess, currentState)



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


testGame :: [Card] -> [(([Card], GameState),Int)]
testGame answer =
  let initialGameState = initialGuess $ length answer
      initialFeedback  = feedback answer (fst initialGameState)
      initialHistory   = [(initialGameState, 1)]
  in  loop initialGameState initialHistory 1 100
 where
  loop
    :: ([Card], GameState)
    -> [(([Card], GameState),Int)]
    -> Int
    -> Int
    -> [(([Card], GameState),Int)]
  loop gs history c cutoff =
    let currentFeedback = feedback answer (fst gs)
        nextGameState   = nextGuess gs currentFeedback
        nextHistory     = history ++ [(nextGameState, c)]
        done = case nextGameState of
          (_, Done {}) -> True
          _ -> False
    in  if c > cutoff || done
          then nextHistory
          else loop nextGameState nextHistory (c + 1) cutoff
