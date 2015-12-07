{-# Language UnicodeSyntax #-}
-- A few simple renamings of functions using unicode characters

module ProbieUnicode where

import qualified Data.Set as S
import Data.List

-- After a while, all things become APL

(↑) ∷ Int → [α] → [α]
(↑) = take

(↓) ∷ Int → [α] → [α]
(↓) = drop

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
  
