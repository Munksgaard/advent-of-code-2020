module Main where

import Control.Exception
import Control.Monad
import Data.Char
import Data.Function
import Data.List ((\\), isSuffixOf, maximumBy, sort)
import Data.Map ((!), Map, fromList, keys)

toSeatId :: Int -> String -> Int
toSeatId acc [] = acc
toSeatId acc ('B' : rest) = toSeatId (acc * 2 + 1) rest
toSeatId acc ('R' : rest) = toSeatId (acc * 2 + 1) rest
toSeatId acc (_ : rest) = toSeatId (acc * 2) rest

main :: IO ()
main = do
  input <- getContents
  let seatIds = sort $ fmap (toSeatId 0) $ lines input
  print $ last seatIds
  zip seatIds (tail seatIds)
    & fmap (\(x, y) -> (x + 1, y - x))
    & maximumBy (compare `on` snd)
    & fst
    & print
