{-# Language UnicodeSyntax #-}

import Prelude.Unicode
import ProbieUnicode
import Crypto.Classes
import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.Char8 as BS

import Text.PrettyPrint

adventCoin ∷ Int → String → ℤ
adventCoin n = fst ∘ head ∘ (dropWhile ((≢ (replicate n '0')) ∘ take n ∘ snd))
             ∘ zip [1..] ∘ map show ∘ map (hash ∘ BS.pack ∷ String → MD5Digest)
             ∘ (zipWith (flip (⧺)) (map show [1..]) ∘ repeat)


part1 ∷ String → ℤ
part1 = adventCoin 5

-- Probably need to compile it, since there will be a memory leak in GHCi
part2 ∷ String → ℤ
part2 = adventCoin 6
        
main ∷ IO ()
main = do
  input ← readFile "P04.txt"
  putRender $ text "Advent of Code Problem 4"
          $+$ text "Part 1:" <+> integer (part1 input)
          $+$ text "Part 2:" <+> integer (part2 input)
