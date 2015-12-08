{-# Language UnicodeSyntax #-}

import Prelude.Unicode
import ProbieUnicode

import Control.Monad.Unicode

import Data.Monoid.Unicode

import Text.PrettyPrint

toMoves ∷ String → [ℤ]
toMoves = map (\ x → if x ≡ '(' then 1 else -1) ∘ filter (∈ "()")

part1 ∷ String → ℤ
part1 = sum ∘ toMoves

part2 ∷ String → ℤ
part2 = σ ∘ length ∘ takeWhile (≥ 0) ∘ scanl (+) 0 ∘ toMoves

main ∷ IO ()
main = do
  input ← readFile "P1.txt"
  putRender $ text "Advent of Code Problem 1"
          $+$ text "Part 1:" <+> integer (part1 input)
          $+$ text "Part 2:" <+> integer (part2 input)
 
