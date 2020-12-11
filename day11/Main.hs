module Main where

import Data.Char
import Data.Function
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Debug.Trace
import Relude.List ((!!?))

allAround :: Int -> Int -> [[Char]] -> [Char]
allAround j i seatmap =
  catMaybes
    [ if j > 0 && i > 0 then Just $ (seatmap !! (j - 1)) !! (i - 1) else Nothing,
      if j > 0 then Just $ (seatmap !! (j - 1)) !! i else Nothing,
      if j > 0 && i < length (head seatmap) - 1 then Just $ (seatmap !! (j - 1)) !! (i + 1) else Nothing,
      if i > 0 then Just $ (seatmap !! j) !! (i - 1) else Nothing,
      if i < length (head seatmap) - 1 then Just $ (seatmap !! j) !! (i + 1) else Nothing,
      if j < length seatmap - 1 && i > 0 then Just $ (seatmap !! (j + 1)) !! (i - 1) else Nothing,
      if j < length seatmap - 1 then Just $ (seatmap !! (j + 1)) !! i else Nothing,
      if j < length seatmap - 1 && i < length (head seatmap) - 1 then Just $ (seatmap !! (j + 1)) !! (i + 1) else Nothing
    ]

unoccupiedAround j i seatmap =
  all
    (/= '#')
    $ allAround j i seatmap

occupiedAround j i seatmap =
  (length $ filter (== '#') $ allAround j i seatmap) >= 4

stabilize :: [[Char]] -> [[Char]]
stabilize seatmap =
  let new =
        fmap
          ( \(cs, j) ->
              fmap
                ( \(c, i) -> case c of
                    'L' -> if unoccupiedAround j i seatmap then '#' else 'L'
                    '#' -> if occupiedAround j i seatmap then 'L' else '#'
                    c -> c
                )
                (zip cs [0 ..])
          )
          (zip seatmap [0 ..])
   in if new == seatmap then seatmap else stabilize new

firstSeat :: [[Char]] -> Int -> Int -> (Int, Int) -> Maybe Char
firstSeat seatmap j i (dy, dx) =
  helper (j + dy) (i + dx)
  where
    helper y x =
      if x >= 0 && x < length (head seatmap)
        && y
        >= 0
        && y
        < length seatmap
        then
          let c = (seatmap !! y) !! x
           in if c == '.'
                then helper (y + dy) (x + dx)
                else Just c
        else Nothing

allAround2 :: Int -> Int -> [[Char]] -> [Char]
allAround2 j i seatmap =
  catMaybes $
    fmap
      (firstSeat seatmap j i)
      [ (-1, -1),
        (-1, 0),
        (-1, 1),
        (0, -1),
        (0, 1),
        (1, -1),
        (1, 0),
        (1, 1)
      ]

stabilize2 :: [[Char]] -> [[Char]]
stabilize2 seatmap =
  let new =
        fmap
          ( \(cs, j) ->
              fmap
                ( \(c, i) -> case c of
                    'L' -> if all (/= '#') $ allAround2 j i seatmap then '#' else 'L'
                    '#' -> if (length $ filter (== '#') $ allAround2 j i seatmap) >= 5 then 'L' else '#'
                    c -> c
                )
                (zip cs [0 ..])
          )
          (zip seatmap [0 ..])
   in if new == seatmap then seatmap else stabilize2 new

main :: IO ()
main = do
  seatmap <- lines <$> getContents
  print $ foldl (+) 0 (fmap (length . filter (== '#')) $ stabilize seatmap)
  print $ foldl (+) 0 (fmap (length . filter (== '#')) $ stabilize2 seatmap)
