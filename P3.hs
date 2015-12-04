{-# Language UnicodeSyntax #-}

import Prelude.Unicode
import qualified Data.Set as S

toDirection ∷ Char → (ℤ,ℤ)
toDirection '^' = (0,1)
toDirection '>' = (1,0)
toDirection '<' = (-1,0)
toDirection 'v' = (0,-1)

path ∷ String → S.Set (ℤ,ℤ)
path = S.fromList ∘
       scanl (\(a',b') x → let (a,b) = toDirection x in (a+a',b+b')) (0,0)

part1 ∷ String → ℤ
part1 = fromIntegral ∘ S.size ∘ path

part2 ∷ String → ℤ
part2 s = fromIntegral $ S.size $ path santaPath `S.union` path roboPath
  where santaPath = [d | (p,d) ← zip (cycle [True,False]) s, p] 
        robotPath = [d | (p,d) ← zip (cycle [False,True]) s, p]
