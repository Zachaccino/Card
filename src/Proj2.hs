-- Replace this comment with your opening documentation.  Leave this module declaration as is:
module Proj2
  ( feedback
  , initialGuess
  , nextGuess
  , GameState
  )
where

import           Card

data GameState = Guessing | Done


feedback :: [Card] -> [Card] -> (Int, Int, Int, Int, Int)
feedback answer guess = (1, 1, 1, 1, 1)

initialGuess :: Int -> ([Card], GameState)
initialGuess size = ([], Done)

nextGuess
  :: ([Card], GameState) -> (Int, Int, Int, Int, Int) -> ([Card], GameState)
nextGuess a b = a

