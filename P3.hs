{-# Language UnicodeSyntax #-}

import Prelude.Unicode
import ProbieUnicode
import qualified Data.Set as S

import Text.PrettyPrint

toDirection ∷ Char → (ℤ,ℤ)
toDirection '^' = (0,1)
toDirection '>' = (1,0)
toDirection '<' = (-1,0)
toDirection 'v' = (0,-1)

path ∷ String → S.Set (ℤ,ℤ)
path = S.fromList ∘
       scanl (\(a',b') x → let (a,b) = toDirection x in (a+a',b+b')) (0,0)

part1 ∷ String → ℤ
part1 = σ ∘ S.size ∘ path

part2 ∷ String → ℤ
part2 s = σ $ S.size $ path santaPath ∪ path robotPath
  where santaPath = [d | (p,d) ← zip (cycle [True,False]) s, p] 
        robotPath = [d | (p,d) ← zip (cycle [False,True]) s, p]

main ∷ IO ()
main = do
  input ← readFile "P3.txt"
  putRender $ text "Advent of Code Problem 3"
          $+$ text "Part 1:" <+> integer (part1 input)
          $+$ text "Part 2:" <+> integer (part2 input)
