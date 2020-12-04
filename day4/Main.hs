module Main where

import Control.Exception
import Control.Monad
import Data.Char
import Data.List ((\\), isSuffixOf)
import Data.Map ((!), Map, fromList, keys)

paragraphs :: String -> [String]
paragraphs [] = []
paragraphs s =
  let (para, rest) = helper [] s
   in para : paragraphs rest
  where
    helper acc [] = (reverse acc, [])
    helper acc ('\n' : '\n' : rest) = (reverse acc, dropWhile isSpace rest)
    helper acc (c : rest) = helper (c : acc) rest

toKeyVal :: String -> (String, String)
toKeyVal s =
  let (x, y) = break (== ':') s
   in (x, tail y)

between :: Int -> Int -> Int -> Bool
between n lower upper = lower <= n && n <= upper

isValid1 :: Map String String -> Bool
isValid1 m =
  ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"] \\ (keys m) == []

isValid2 :: Map String String -> IO Bool
isValid2 m =
  catch (evaluate helper) (\x -> let _ = x :: SomeException in return False)
  where
    helper =
      between (read (m ! "byr")) 1920 2002
        && between (read (m ! "iyr")) 2010 2020
        && between (read (m ! "eyr")) 2020 2030
        && ( ("cm" `isSuffixOf` (m ! "hgt") && between (read $ take 3 (m ! "hgt")) 150 193)
               || ("in" `isSuffixOf` (m ! "hgt") && between (read $ take 2 (m ! "hgt")) 59 76)
           )
        && isHairColor (m ! "hcl")
        && isEyeColor (m ! "ecl")
        && isPassportID (m ! "pid")
    isHairColor ('#' : rest) = all isHexDigit rest
    isHairColor _ = False
    isEyeColor s = s `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    isPassportID s = length s == 9 && all isDigit s

main :: IO ()
main = do
  input <- getContents
  let maps = fmap (fromList . fmap toKeyVal . words) $ paragraphs input
  print $ length $ filter isValid1 maps
  valids <- filterM isValid2 maps
  print $ length $ valids
  return ()
