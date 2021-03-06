{-# Language UnicodeSyntax #-}

import Prelude.Unicode
import ProbieUnicode

import Control.Applicative
import Control.Applicative.Unicode
  
import Data.List
import Data.Map (Map)
import Data.Maybe

import qualified Data.Map as M

import Text.PrettyPrint
  
distance ∷ [String] → (String,String,ℤ)
distance [s, _, d, _, n] = (s,d,read n)

getPlaces ∷ [(String, String, ℤ)] → Map String ℤ
getPlaces = foldl' (\m (k,v) → M.insert v k m) M.empty ∘ zip [1..] ∘ nub
          ∘ concatMap (\(x,y,_) → [x,y])

getPaths ∷ [(String, String, ℤ)] → Map String ℤ → Map ℤ [(ℤ, ℤ)]
getPaths paths places = foldl' (\m (s,d,v) → M.insertWith (⧺) s [(d,v)] m)
                               M.empty
                      ∘ map (\(x,y,z) → (places M.! x, places M.! y,z)) $ paths

allPaths ∷ [(String, String, ℤ)] → [ℤ]
allPaths paths = catMaybes $ map (`followPath` (getPaths paths places))
                 (permutations $ (M.elems places))
  where places = getPlaces paths

followPath ∷ [ℤ] → Map ℤ [(ℤ, ℤ)] → Maybe ℤ
followPath []  _ = Just 0
followPath [_] _ = Just 0
followPath (x:y:zs) world = do
    paths ← M.lookup x world
    n     ← lookup y paths
    m     ← followPath (y:zs) world
    Just $ n + m

reflexive ∷ [(String,String,ℤ)] → [(String,String,ℤ)]
reflexive xs = xs ⧺ map (\(x,y,z) → (y,x,z)) xs

generatePaths ∷ String → [ℤ]
generatePaths = allPaths ∘ reflexive ∘ map distance ∘ map words ∘ lines
 
part1 ∷ String → ℤ
part1 = minimum ∘ generatePaths

part2 ∷ String → ℤ
part2 = maximum ∘ generatePaths

main ∷ IO ()
main = do
  input ← readFile "P09.txt"
  putRender $ text "Advent of Code Problem 9"
          $+$ text "Part 1:" <+> integer (part1 input)
          $+$ text "Part 2:" <+> integer (part2 input)
