{-# Language UnicodeSyntax #-}

import Prelude.Unicode
import ProbieUnicode

import Control.Applicative
import Control.Applicative.Unicode
import Data.List

moreThan ∷ Int → [α] → Bool
moreThan n = not ∘ null ∘ (pred n ↓)

nGroups ∷ Int → [α] → [[α]]
nGroups n = filter (moreThan n) ∘ map (n ↑) ∘ tails

niceString ∷ String → Bool
niceString = (∧) <$> (moreThan 3) ∘ filter (∈ "aeiou") ⊛
               (((∧) <$> any (\[x,y] → x ≡ y)
                       ⊛ all (∉ ["ab", "cd", "pq", "xy"])) ∘ nGroups 2)

part1 ∷ String → ℤ
part1 = fromIntegral ∘ length ∘ filter niceString ∘ lines

nonRepeatedPairs ∷ [String] → Bool
nonRepeatedPairs = or ∘ (zipWith (∈) <$> id ⊛ (2 ↓) ∘ tails)

improvedNiceString ∷ String → Bool
improvedNiceString = (∧) <$> nonRepeatedPairs ∘ nGroups 2
                           ⊛ any (\[x,_,y] → x ≡ y) ∘ nGroups 3

part2 ∷ String → ℤ
part2 = fromIntegral ∘ length ∘ filter improvedNiceString ∘ lines
