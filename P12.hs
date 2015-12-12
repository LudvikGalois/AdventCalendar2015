{-# Language UnicodeSyntax #-}

import Prelude.Unicode
import ProbieUnicode

import Control.Applicative
import Control.Applicative.Unicode
  
import Text.PrettyPrint
import Text.JSON

part1 ∷ String → ℤ
part1 s = case countNums <$> (decode s) of
  Ok x -> round x

part2 ∷ String → ℤ
part2 s = case countNums' <$> (decode s) of
  Ok x -> round x

countNums ∷ JSValue → ℚ
countNums x = case x of
  JSRational _ n → n
  JSArray xs → sum (map countNums xs)
  JSObject obj → sum $ map (countNums ∘ snd) $ fromJSObject obj
  _ → 0

countNums' ∷ JSValue → ℚ
countNums' x = case x of
  JSRational _ n → n
  JSArray xs → sum (map countNums' xs)
  JSObject obj | JSString (toJSString "red") ∈ (map snd $ fromJSObject obj) → 0
               | otherwise → sum $ map (countNums' ∘ snd) $ fromJSObject obj
  _ → 0
