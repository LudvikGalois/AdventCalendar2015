{-# Language UnicodeSyntax #-}

import Prelude.Unicode
import ProbieUnicode

import Control.Applicative
import Control.Applicative.Unicode

import Data.Bits
import Data.Char
import Data.List
import Data.Map (Map)
import Data.Word

import Text.PrettyPrint

import qualified Data.Map as M

type Wireset = Map String Word16
type Wirespec = (String, (Wireset → Word16))

δ x m | all isDigit x = read x
      | otherwise     = m M.! x

parseWirespec ∷ String → Wirespec
parseWirespec s = case words s of
  [x, "AND",    y, "->", w] → (w, (.&.) <$> δ x ⊛ δ y)
  [x, "OR",     y, "->", w] → (w, (.|.) <$> δ x ⊛ δ y)
  [x, "LSHIFT", n, "->", w] → (w, (`shift` (read n)) ∘ δ x)
  [x, "RSHIFT", n, "->", w] → (w, (`shift` (- (read n)))∘ δ x)
  ["NOT", x, "->", w]       → (w, complement ∘ δ x)
  [n, "->", w]              → (w, δ n)

löb ∷ Functor f ⇒ f (f α → α) → f α
löb f = go
  where go = fmap ($ go) f

buildWireSet ∷ String → Wireset
buildWireSet = löb ∘ buildWireSpec

buildWireSpec ∷ String → Map String (Wireset → Word16)
buildWireSpec = foldl' (\m (k,v) → M.insert k v m) M.empty
              ∘ map parseWirespec ∘ lines

part1 ∷ String → Word16
part1 = (M.! "a") ∘ buildWireSet

part2 ∷ String → Word16
part2 = (M.! "a") ∘ löb ∘ ((M.insert "b") <$> const ∘ part1 ⊛ buildWireSpec)

main ∷ IO ()
main = do
  input ← readFile "P7.txt"
  putRender $ text "Advent of Code Problem 7"
          $+$ text "Part 1:" <+> integer (σ $ part1 input)
          $+$ text "Part 2:" <+> integer (σ $ part2 input)
