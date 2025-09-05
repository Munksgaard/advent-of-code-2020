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

traceWith s a = trace (s ++ ": " ++ show a) a

parseLine :: String -> (Char, Int)
parseLine (c : rest) = (c, read rest)
parseLine _ = undefined

data Heading = East | South | West | North

forward :: (Int, Int) -> Heading -> Int -> (Int, Int)
forward (x, y) East n = (x + n, y)
forward (x, y) South n = (x, y + n)
forward (x, y) West n = (x - n, y)
forward (x, y) North n = (x, y - n)

left :: Heading -> Heading
left East = North
left South = East
left West = South
left North = West

right :: Heading -> Heading
right = left . left . left

turn :: Char -> Int -> Heading -> Heading
turn _ 0 h = h
turn 'R' n h = turn 'R' (n - 90) $ right h
turn 'L' n h = turn 'L' (n - 90) $ left h

navigate :: (Int, Int) -> Heading -> [(Char, Int)] -> (Int, Int)
navigate pos _ [] = pos
navigate (x, y) h (('E', n) : rest) = navigate (x + n, y) h rest
navigate (x, y) h (('S', n) : rest) = navigate (x, y + n) h rest
navigate (x, y) h (('W', n) : rest) = navigate (x - n, y) h rest
navigate (x, y) h (('N', n) : rest) = navigate (x, y - n) h rest
navigate pos h (('F', n) : rest) = navigate (forward pos h n) h rest
navigate pos h ((c, n) : rest) = navigate pos (turn c n h) rest

turn2 :: (Char, Int) -> (Int, Int) -> (Int, Int)
turn2 (_, 0) wp = wp
turn2 ('R', n) (dx, dy) = turn2 ('R', n - 90) (- dy, dx)
turn2 (c, n) h = turn2 (c, n - 90) $ turn2 ('R', 270) h

navigate2 :: (Int, Int) -> (Int, Int) -> [(Char, Int)] -> (Int, Int)
navigate2 pos _ [] = pos
navigate2 (x, y) (dx, dy) (('F', n) : rest) =
  navigate2 (x + dx * n, y + dy * n) (dx, dy) rest
navigate2 pos (dx, dy) (('E', n) : rest) =
  navigate2 pos (dx + n, dy) rest
navigate2 pos (dx, dy) (('S', n) : rest) =
  navigate2 pos (dx, dy + n) rest
navigate2 pos (dx, dy) (('W', n) : rest) =
  navigate2 pos (dx - n, dy) rest
navigate2 pos (dx, dy) (('N', n) : rest) =
  navigate2 pos (dx, dy - n) rest
navigate2 pos wp (c : rest) =
  navigate2 pos (turn2 c wp) rest

main :: IO ()
main = do
  instrs <- fmap parseLine . lines <$> getContents
  print $ uncurry (+) $ navigate (0, 0) East instrs
  print $ uncurry (+) $ navigate2 (0, 0) (10, -1) instrs
