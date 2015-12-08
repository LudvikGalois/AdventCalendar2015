{-# Language UnicodeSyntax #-}

import Prelude.Unicode
import ProbieUnicode

import Control.Applicative
import Control.Applicative.Unicode

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
