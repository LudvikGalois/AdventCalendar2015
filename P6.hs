{-# Language UnicodeSyntax #-}

import Prelude.Unicode
import ProbieUnicode

import Data.Array
import Data.List
  
import Text.PrettyPrint

-- Note, immutable arrays make this really slow, except mutable
-- arrays add too much boiler plate, and I was trying to write this
-- quickly so I could get onto the leaderboard

type Instruction = ((Int → Int), (Int,Int), (Int,Int))

readT ∷ String → (Int,Int)
readT n = read ('(' : n ⧺ ")")

toInstruction ∷ ((Int → Int), (Int → Int), (Int → Int)) → [String] → Instruction
toInstruction (tog, _, _)  ["toggle",  n, _, m] = (tog, readT n, readT m)
toInstruction (_, on, off) ["turn", s, n, _, m] = (p  , readT n, readT m)
  where p = case s of
          "on"  → on
          "off" → off

applyInstruction ∷ Array (Int,Int) Int → Instruction → Array (Int,Int) Int
applyInstruction arr (f, (x1,y1), (x2,y2)) =
    arr // [((x,y),f (arr ! (x,y))) | x ← [x1..x2], y ← [y1..y2]]
 
solve ∷ (Int → Int, Int → Int, Int → Int) → String → Int
solve instr = sum ∘ elems ∘ foldl' applyInstruction initialLights
            ∘ map (toInstruction instr)
            ∘ map words ∘ lines

initialLights ∷ Array (Int,Int) Int
initialLights = array ((0,0),(999,999)) [((x,y),0) | x ← [0..999], y ← [0..999]]

part1 ∷ String → Int
part1 = solve (\n → if n ≡ 0 then 1 else 0, const 1, const 0)

part2 ∷ String → Int
part2 = solve (succ ∘ succ, succ, max 0 ∘ pred)
        
main ∷ IO ()
main = do
  input ← readFile "P6.txt"
  putRender $ text "Advent of Code Problem 6"
          $+$ text "Part 1:" <+> int (part1 input)
          $+$ text "Part 2:" <+> int (part2 input)
