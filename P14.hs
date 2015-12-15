{-# Language UnicodeSyntax #-}

import Prelude.Unicode
import ProbieUnicode

import Text.PrettyPrint

import Data.Ord
import Data.List

import Data.Stream (Stream)
import qualified Data.Stream as S

data Reindeer = Reindeer { name ∷ String
                         , distance ∷ (Stream ℤ) -- Reindeer never sleep
                         }

type Race = [Reindeer]

at ∷ Reindeer → Int → ℤ
at r n = (distance r) S.!! n

runRace ∷ Int → Race → Reindeer
runRace raceLength = maximumBy (comparing ((S.!! raceLength) ∘ distance))

makeReindeer ∷ String → Reindeer
makeReindeer s = case words s of
  [name, _, _, distS, _, _, durationS, _, _, _, _, _, _, restS, _] → 
    Reindeer name (S.scan' (\n p → if p then n+dist else n) 0
                    (S.cycle (replicate duration True ⧺ replicate rest False)))
      where duration = read durationS
            dist     = read distS
            rest     = read restS

makeRace ∷ String → Race
makeRace = map makeReindeer ∘ lines

part1 ∷ String → ℤ
part1 = (S.!! raceTime) ∘ distance ∘ runRace raceTime ∘ makeRace
  where raceTime = 2503

-- Most unexpected part 2, most were trivial changes.
-- I'll write this properly later
part2 ∷ String → ℤ
part2 s = σ $ maximum $ map length $ group $ sort $ map name $ concat
        $ [filter (\r → (r `at` n) ≡ best) rein | n <- [1..raceTime]
              , let best = (runRace n rein) `at` n]
  where raceTime = 2503
        rein = makeRace s

main ∷ IO ()
main = do
  input ← readFile "P14.txt"
  putRender $ text "Advent of Code Problem 14"
          $+$ text "Part 1:" <+> integer (part1 input)
          $+$ text "Part 2:" <+> integer (part2 input)
