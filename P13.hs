{-# Language UnicodeSyntax #-}

import Prelude.Unicode
import ProbieUnicode

import Control.Applicative
import Control.Applicative.Unicode

import Data.List
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe

import Text.PrettyPrint

processInput ∷ String → Map String [(String,Integer)]
processInput = foldl' (\m (s,v,d) → M.insertWith (⧺) s [(d,v)] m) M.empty
             ∘ map processLine ∘ map words ∘ lines
  where processLine [p1,"would",sign,n,"happiness"
                    ,"units","by","sitting","next","to",p2] = (p1,s' (read n),init p2)
          where s' = if sign == "gain" then id else negate

tableValue m (x:xs) = tableValue_ m (x:xs++[x])

tableValue_ :: Map String [(String,Integer)] → [String] → Integer
tableValue_ m [x,y] = fromJust $ (+) <$> lookup y (m M.! x) ⊛ lookup x (m M.! y)
tableValue_ m (x:y:zs) = tableValue_ m [x,y] + tableValue_ m (y:zs)

part1 ∷ String → ℤ
part1 s = maximum $ map (tableValue values) $ permutations $ M.keys $ values
    where values = processInput s

-- It said insert yourself
part2 ∷ String → ℤ
part2 s = maximum $ map (tableValue values') $ permutations $ M.keys $ values'
    where values' = foldl' (\m x → M.insertWith (⧺) x [("Probie",0)]
                            $ M.insertWith (⧺) "Probie" [(x,0)] m) values people
          people = M.keys values
          values = processInput s
          
main ∷ IO ()
main = do
  input ← readFile "P13.txt"
  putRender $ text "Advent of Code Problem 13"
          $+$ text "Part 1:" <+> integer (part1 input)
          $+$ text "Part 2:" <+> integer (part2 input)
