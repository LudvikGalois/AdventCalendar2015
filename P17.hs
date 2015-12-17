{-# Language UnicodeSyntax #-}

import Prelude.Unicode
import ProbieUnicode

import Text.PrettyPrint
  
combs ∷ [a] → [[a]]
combs [] = [[]]
combs (x:xs) = map (x:) (combs xs) ++ combs xs

part1 ∷ String → ℤ
part1 = σ ∘ length ∘ filter ((≡ 150) ∘ sum) ∘ combs ∘ map read ∘ lines

part2 ∷ String → ℤ
part2 s = σ $ length $ filter (≡ mincontainers) $ eggnog
  where eggnog = map length $ filter ((≡ 150) ∘ sum)
               $ combs $ map read $ lines s
        mincontainers = minimum eggnog 

main ∷ IO ()
main = do
  input ← readFile "P17.txt"
  putRender $ text "Advent of Code Problem 17"
          $+$ text "Part 1:" <+> integer (part1 input)
          $+$ text "Part 2:" <+> integer (part2 input)
