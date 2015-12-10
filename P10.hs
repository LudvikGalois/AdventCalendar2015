{-# Language UnicodeSyntax #-}

import Prelude.Unicode
import ProbieUnicode

import Text.PrettyPrint
import Data.List

process ∷ String → String
process = foldr (\n acc → (show n) ⧺ acc) []
        ∘ concatMap (\x → [length x, read [head x]]) ∘ group 

lookandsay ∷ [String]
lookandsay = iterate process "1321131112"

part1 ∷ ℤ
part1 = σ ∘ length $ lookandsay ‼ 40

part2 ∷ ℤ
part2 = σ ∘ length $ lookandsay ‼ 50

main ∷ IO ()
main = do
  putRender $ text "Advent of Code Problem 10"
          $+$ text "Part 1:" <+> integer part1 
          $+$ text "Part 2:" <+> integer part2 
