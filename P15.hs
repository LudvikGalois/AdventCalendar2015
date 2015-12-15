{-# Language UnicodeSyntax #-}

import Prelude.Unicode
import ProbieUnicode

import Text.PrettyPrint

data Ingredient = Ingredient { name ∷ String
                             , capacity ∷ ℤ
                             , durability ∷ ℤ
                             , flavor ∷ ℤ
                             , texture ∷ ℤ
                             , calories ∷ ℤ
                             }
  deriving (Show, Eq)

parseIngredient ∷ String → Ingredient
parseIngredient s = case words s of
  [name, _, cap, _, dur, _, flav,_ , tex,_ , cal] →
    Ingredient name' cap' dur' flav' tex' cal'
      where cap' = read $ init cap
            dur' = read $ init dur
            flav' = read $ init flav
            tex' = read $ init tex
            cal' = read $ cal
            name' = init name

parseIngredients ∷ String → [Ingredient]
parseIngredients = map parseIngredient ∘ lines

recipe ∷ ℤ → [Ingredient] → [[Ingredient]]
recipe _ [] = error "No ingredients"
recipe 0 _ = []
recipe n [x] = [replicate (σ n) x]
recipe n (x:xs) = map (x:) $ recipe (n-1) (x:xs) ++ recipe (n-1) xs

value ∷ Bool → [Ingredient] → ℤ
value part2 recipe = if all (≥ 0) [caps,durs,flavs,texts]
                        ∧ (not part2 ∨ cals ≡ 500)
                     then product [caps,durs,flavs,texts] else 0
  where caps = sum $ map capacity recipe
        durs = sum $ map durability recipe
        flavs = sum $ map flavor recipe
        texts = sum $ map texture recipe
        cals = sum $ map calories recipe
        
part1 ∷ String → ℤ
part1 = maximum ∘ map (value False) ∘ recipe 100 ∘ parseIngredients

part2 ∷ String → ℤ
part2 = maximum ∘ map (value True) ∘ recipe 100 ∘ parseIngredients

main ∷ IO ()
main = do
  input ← readFile "P15.txt"
  putRender $ text "Advent of Code Problem 15"
          $+$ text "Part 1:" <+> integer (part1 input)
          $+$ text "Part 2:" <+> integer (part2 input)
