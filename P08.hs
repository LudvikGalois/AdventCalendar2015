{-# Language UnicodeSyntax #-}

import Prelude.Unicode
import ProbieUnicode

import Control.Applicative
import Control.Applicative.Unicode

import Text.PrettyPrint

stripEscaped ∷ String → String
stripEscaped [] = []
stripEscaped ('\\':'\\':xs)    = '\\': stripEscaped xs
stripEscaped ('\\':'"':xs)     = '"' : stripEscaped xs
stripEscaped ('\\':'x':_:_:xs) = '_' : stripEscaped xs -- Good enough
stripEscaped (x:xs)            = x   : stripEscaped xs

part1 ∷ String → Int
part1 = sum ∘ map ((-) <$> length ⊛ length ∘ tail ∘ init ∘ stripEscaped) ∘ lines

part2 ∷ String → Int
part2 = sum ∘ map ((-) <$> length ∘ show ⊛ length) ∘ lines 
        
main ∷ IO ()
main = do
  input ← readFile "P08.txt"
  putRender $ text "Advent of Code Problem 8"
          $+$ text "Part 1:" <+> int (part1 input)
          $+$ text "Part 2:" <+> int (part2 input)
