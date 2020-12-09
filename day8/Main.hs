module Main where

import Data.Char
import Data.Function
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Debug.Trace

traceWith :: Show a => String -> a -> a
traceWith s a = trace (s ++ ": " ++ show a) a

data Instruction
  = Acc Int
  | Nop Int
  | Jmp Int
  deriving (Show)

parseLine :: String -> Instruction
parseLine ('n' : _ : _ : _ : '+' : rest) = Nop $ read rest
parseLine ('n' : _ : _ : _ : rest) = Nop $ read rest
parseLine ('j' : _ : _ : _ : '+' : rest) = Jmp $ read rest
parseLine ('j' : _ : _ : _ : rest) = Jmp $ read rest
parseLine ('a' : _ : _ : _ : '+' : rest) = Acc $ read rest
parseLine ('a' : _ : _ : _ : rest) = Acc $ read rest
parseLine _ = undefined

eval :: [Instruction] -> Int -> Int -> S.Set Int -> Int
eval instrs pc acc alreadyRun =
  if pc `S.member` alreadyRun
    then acc
    else case instrs !! pc of
      Jmp i -> eval instrs (pc + i) acc (S.insert pc alreadyRun)
      Acc i -> eval instrs (pc + 1) (acc + i) (S.insert pc alreadyRun)
      Nop i -> eval instrs (pc + 1) acc (S.insert pc alreadyRun)

eval2 :: Int -> Int -> S.Set Int -> [Instruction] -> Maybe Int
eval2 pc acc alreadyRun instrs =
  if pc `S.member` alreadyRun || pc < 0
    then Nothing
    else case (compare pc (length instrs), instrs !! pc) of
      (EQ, _) -> Just acc
      (GT, _) -> Nothing
      (_, Jmp i) -> eval2 (pc + i) acc (S.insert pc alreadyRun) instrs
      (_, Acc i) -> eval2 (pc + 1) (acc + i) (S.insert pc alreadyRun) instrs
      (_, Nop i) -> eval2 (pc + 1) acc (S.insert pc alreadyRun) instrs

changeOne :: [Instruction] -> [[Instruction]]
changeOne [] = []
changeOne (x@(Nop i) : xs) =
  (Jmp i : xs) : (fmap (x :) $ changeOne xs)
changeOne (x@(Jmp i) : xs) =
  (Nop i : xs) : (fmap (x :) $ changeOne xs)
changeOne (x : xs) =
  fmap (x :) $ changeOne xs

main :: IO ()
main = do
  instrs <- fmap parseLine . lines <$> getContents
  print $ eval instrs 0 0 mempty
  print $ head $ mapMaybe (eval2 0 0 mempty) $ changeOne instrs
