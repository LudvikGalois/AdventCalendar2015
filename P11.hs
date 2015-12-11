{-# Language UnicodeSyntax #-}

import Control.Applicative
import Control.Applicative.Unicode

import Prelude.Unicode
import ProbieUnicode

import Data.List

import Text.PrettyPrint

increment ∷ String → String
increment = reverse ∘ incr ∘ reverse
  where incr ('z':xs) = 'a'    : incr xs
        incr (x:xs)   = succ x : xs

moreThan ∷ Int → [α] → Bool
moreThan n = not ∘ null ∘ (pred n ↓)

nGroups ∷ Int → [α] → [[α]]
nGroups n = filter (moreThan n) ∘ map (n ↑) ∘ tails

increasing ∷ String → Bool
increasing [x,y,z] = succ x ≡ y ∧ pred z ≡ y

twoPairs ∷ [String] → ℤ
twoPairs ([x,y]:xs) | x ≡ y     = 1 + twoPairs (1 ↓ xs)
                    | otherwise = twoPairs xs
twoPairs _ = 0

valid ∷ String → Bool
valid = (`all` [ nGroups 2 ⨾ twoPairs ⨾ (≥ 2) -- Happy Daniel?
               , nGroups 3 ⨾ any increasing 
               , all (∉ "iol") 
               ]) ∘ (flip ($))

part1 ∷ String
part1 = head ∘ dropWhile (not ∘ valid) $ iterate increment "cqjxjnds"

part2 :: String
part2 = head ∘ dropWhile (not ∘ valid) $ iterate increment part1

main ∷ IO ()
main = do
  putRender $ text "Advent of Code Problem 11"
          $+$ text "Part 1:" <+> text part1
          $+$ text "Part 2:" <+> text part2
