module Main where

import Data.Char
import Data.Function
import Data.List

paragraphs :: String -> [String]
paragraphs [] = []
paragraphs s =
  let (para, rest) = helper [] s
   in para : paragraphs rest
  where
    helper acc [] = (reverse acc, [])
    helper acc ('\n' : '\n' : rest) = (reverse acc, dropWhile isSpace rest)
    helper acc (c : rest) = helper (c : acc) rest

main :: IO ()
main = do
  input <- getContents
  print $ sum $ fmap (length . nub . filter isAlpha) $ paragraphs input
  paragraphs input
    & fmap (length . (\ws -> filter (\c -> all (c `elem`) ws) ['a' .. 'z']) . words)
    & sum
    & print
