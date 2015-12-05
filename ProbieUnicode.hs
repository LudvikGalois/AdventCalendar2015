{-# Language UnicodeSyntax #-}
-- A few simple renamings of functions using unicode characters

module ProbieUnicode where

-- After a while, all things become APL

(↑) ∷ Int → [α] → [α]
(↑) = take

(↓) ∷ Int → [α] → [α]
(↓) = drop
