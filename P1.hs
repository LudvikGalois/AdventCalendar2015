{-# Language UnicodeSyntax #-}

import Prelude.Unicode

toMoves ∷ String → [ℤ]
toMoves = map (\ x → if x == '(' then 1 else -1) ∘ filter (∈ "()")

part1 ∷ String → ℤ
part1 = sum ∘ toMoves

part2 ∷ String → ℤ
part2 = fromIntegral ∘ length ∘ takeWhile (≥ 0) ∘ scanl (+) 0 ∘ toMoves

