{-# Language UnicodeSyntax #-}

import Prelude.Unicode
import Crypto.Classes
import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.Char8 as BS

part1 ∷ String → ℤ
part1 = fst ∘ head ∘ (dropWhile ((/=  "00000") ∘ take 5 ∘ snd)) ∘ zip [1..]
      ∘ map show ∘ map (hash ∘ BS.pack ∷ String → MD5Digest)
      ∘ (zipWith (flip (++)) (map show [1..]) ∘ repeat)

-- Probably need to compile it, since there will be a memory leak in GHCi
part2 ∷ String → ℤ
part2 = fst ∘ head ∘ (dropWhile ((/=  "000000") ∘ take 6 ∘ snd)) ∘ zip [1..]
      ∘ map show ∘ map (hash ∘ BS.pack ∷ String → MD5Digest)
      ∘ (zipWith (flip (++)) (map show [1..]) ∘ repeat)

