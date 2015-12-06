{-# Language UnicodeSyntax #-}
-- A few simple renamings of functions using unicode characters

module ProbieUnicode where

import qualified Data.Set as S

-- After a while, all things become APL

(↑) ∷ Int → [α] → [α]
(↑) = take

(↓) ∷ Int → [α] → [α]
(↓) = drop

σ ∷ (Integral α, Integral β) ⇒ α → β
σ = fromIntegral

(∪) ∷ Ord α ⇒ S.Set α → S.Set α → S.Set α
(∪) = S.union

(∩) ∷ Ord α ⇒ S.Set α → S.Set α → S.Set α
(∩) = S.intersection
