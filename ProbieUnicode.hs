{-# Language UnicodeSyntax #-}
-- A few simple renamings of functions using unicode characters

module ProbieUnicode where

import qualified Data.Set as S
import Data.List

import Prelude.Unicode

import Text.PrettyPrint

-- After a while, all things become APL

(↑) ∷ Int → [α] → [α]
(↑) = take

(↓) ∷ Int → [α] → [α]
(↓) = drop

(‼) ∷ [α] → Int → α
(‼) = (!!)

(⨾) ∷ (α → β) → (β → γ) → α → γ
(⨾) = flip (∘)

σ ∷ (Integral α, Integral β) ⇒ α → β
σ = fromIntegral

class SetLike s where
  (∪) ∷ Ord α ⇒ s α → s α → s α 
  (∩) ∷ Ord α ⇒ s α → s α → s α 

instance SetLike S.Set where
  (∪) = S.union
  (∩) = S.intersection

instance SetLike ([]) where
  (∪) = union
  (∩) = intersect

-- Doesn't really belong here, but I don't feel like making a separate file
-- for it
putRender ∷ Doc → IO ()
putRender = putStrLn ∘ render
