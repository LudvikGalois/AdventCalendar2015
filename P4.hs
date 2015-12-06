{-# Language UnicodeSyntax #-}

import Prelude.Unicode
import Crypto.Classes
import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.Char8 as BS

adventCoin ∷ Int → String → ℤ
adventCoin n = fst ∘ head ∘ (dropWhile ((/=  (replicate n '0')) ∘ take n ∘ snd))
             ∘ zip [1..] ∘ map show ∘ map (hash ∘ BS.pack ∷ String → MD5Digest)
             ∘ (zipWith (flip (++)) (map show [1..]) ∘ repeat)


part1 ∷ String → ℤ
part1 = adventCoin 5

-- Probably need to compile it, since there will be a memory leak in GHCi
part2 ∷ String → ℤ
part2 = adventCoin 6
