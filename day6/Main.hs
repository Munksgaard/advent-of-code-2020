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
  input <- paragraphs <$> getContents
  print $ sum $ fmap (length . nub . filter isAlpha) input
  print $ sum $ fmap (length . foldr intersect ['a' .. 'z'] . words) input
