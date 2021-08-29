{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Scrabble where

import Data.Char (toUpper)
import Data.Semigroup

newtype Score =
  Score Int
  deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i

instance Monoid Score where
  mempty = Score 0
  mappend = (+)

instance Semigroup Score where
  (<>) = (+)

score :: Char -> Score
score c
  | toUpper c `elem` ['A', 'E', 'I', 'L', 'N', 'O', 'R', 'S', 'T', 'U'] =
    Score 1
  | toUpper c `elem` ['D', 'G'] = Score 2
  | toUpper c `elem` ['B', 'C', 'M', 'P'] = Score 3
  | toUpper c `elem` ['F', 'H', 'V', 'W', 'Y'] = Score 4
  | toUpper c == 'K' = Score 5
  | toUpper c `elem` ['J', 'X'] = Score 8
  | toUpper c `elem` ['Z', 'Q'] = Score 10
  | otherwise = Score 0

scoreString :: String -> Score
scoreString = sum . map score
