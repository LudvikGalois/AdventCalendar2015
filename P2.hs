{-# Language UnicodeSyntax #-}

import Prelude.Unicode
import Data.List.Split
import Data.List

dimensions ∷ String → (ℤ, ℤ, ℤ)
dimensions = (\[x,y,z] → (x,y,z)) ∘ sort ∘ map read ∘ splitOn "x"

wrappingPaper ∷ (ℤ,ℤ,ℤ) → ℤ
wrappingPaper (l,w,h) = 2*l*w+2*w*h+2*h*l+l*w

ribbon  ∷ (ℤ,ℤ,ℤ) → ℤ
ribbon (l,w,h) = 2*l+2*w+l*w*h

part1 ∷ String → ℤ
part1 = sum ∘ map wrappingPaper ∘ map dimensions ∘ lines

part2 ∷ String → ℤ
part2 = sum ∘ map ribbon ∘ map dimensions ∘ lines
