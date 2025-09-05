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
import Text.ParserCombinators.ReadP

data Instr
  = Mask [Char]
  | Mem Int Int

parseString :: String -> [Instr]
parseString s = case readP_to_S pProgram s of
  [(instrs, "")] -> instrs
  _ -> undefined

type Parser p = ReadP p

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

pProgram = undefined

whitespace = undefined

main :: IO ()
main = do
  print "Day14"
