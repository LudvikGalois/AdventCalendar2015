{-# Language UnicodeSyntax #-}

import Prelude.Unicode
import ProbieUnicode

import Control.Applicative
import Control.Applicative.Unicode
import Data.List

import Text.PrettyPrint

moreThan ∷ Int → [α] → Bool
moreThan n = not ∘ null ∘ (pred n ↓)

nGroups ∷ Int → [α] → [[α]]
nGroups n = filter (moreThan n) ∘ map (n ↑) ∘ tails

niceString ∷ String → Bool
niceString = (∧) <$> (moreThan 3) ∘ filter (∈ "aeiou") ⊛
               (((∧) <$> any (\[x,y] → x ≡ y)
                       ⊛ all (∉ ["ab", "cd", "pq", "xy"])) ∘ nGroups 2)

part1 ∷ String → ℤ
part1 = σ ∘ length ∘ filter niceString ∘ lines

nonRepeatedPairs ∷ [String] → Bool
nonRepeatedPairs = or ∘ (zipWith (∈) <$> id ⊛ (2 ↓) ∘ tails)

improvedNiceString ∷ String → Bool
improvedNiceString = (∧) <$> nonRepeatedPairs ∘ nGroups 2
                           ⊛ any (\[x,_,y] → x ≡ y) ∘ nGroups 3

part2 ∷ String → ℤ
part2 = σ ∘ length ∘ filter improvedNiceString ∘ lines

main ∷ IO ()
main = do
  input ← readFile "P05.txt"
  putRender $ text "Advent of Code Problem 5"
          $+$ text "Part 1:" <+> integer (part1 input)
          $+$ text "Part 2:" <+> integer (part2 input)
