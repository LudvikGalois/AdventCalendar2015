{-# Language UnicodeSyntax #-}

import Prelude.Unicode
import ProbieUnicode

import Text.PrettyPrint
import Data.List.Split

data Aunt = Aunt { number ∷  ℤ
                 , children ∷ Maybe ℤ
                 , cats ∷ Maybe ℤ
                 , samoyeds ∷ Maybe ℤ
                 , pomeranians ∷ Maybe ℤ
                 , akitas ∷ Maybe ℤ
                 , vizslas ∷ Maybe ℤ
                 , goldfish ∷ Maybe ℤ
                 , trees ∷ Maybe ℤ
                 , cars ∷ Maybe ℤ
                 , perfumes ∷ Maybe ℤ
                 }
  deriving Show
instance Num a => Num (Maybe a) where
  fromInteger = Just . fromInteger -- Save typing

goodAunt ∷ Aunt
goodAunt = Aunt 0 3 7 2 3 0 0 5 3 2 1

readAunt ∷ String → Aunt
readAunt s = Aunt (read $ takeWhile (≠ ':') $ drop 4 s) child cats samo
               pomer akit vizsl gold trees cars perfs
  where s' = map (\[x,y] → (x, read y)) $ map (splitOn ": ")
           $ splitOn ", " $ tail $ tail $ dropWhile (≠ ':') s
        child = lookup "children" s'
        cats = lookup "cats" s'
        samo = lookup "samoyeds" s'
        pomer = lookup "pomeranians" s'
        akit = lookup "akitas" s'
        vizsl = lookup "vizslas" s'
        gold = lookup "goldfish" s'
        trees = lookup "trees" s'
        cars = lookup "cars" s'
        perfs = lookup "perfumes" s'

possibleAunt ∷ Aunt → Bool
possibleAunt (Aunt _ ch ca sa po ak vi go tr car pe) =
  let (Aunt _ ch' ca' sa' po' ak' vi' go' tr' car' pe') = goodAunt
  in all possibleMatch [ (ch,ch'),(ca,ca'), (sa,sa'), (po,po'), (ak,ak')
                       , (vi,vi'), (go,go'), (tr,tr'), (car,car'),(pe,pe')]
  where possibleMatch (Nothing,_) = True
        possibleMatch (x,y) = x ≡ y

possibleAunt' ∷ Aunt → Bool
possibleAunt'(Aunt _ ch ca sa po ak vi go tr car pe) =
  let (Aunt _ ch' ca' sa' po' ak' vi' go' tr' car' pe') = goodAunt
  in all possibleMatch [ (ch,ch'),(sa,sa'), (ak,ak'), (vi,vi')
                       , (car,car'),(pe,pe')] ∧
     all possibleGreater [(ca,ca'),(tr,tr')] ∧
     all possibleLess [(po,po'),(go,go')]
  where possibleMatch (Nothing,_) = True
        possibleMatch (x,y) = x ≡ y
        possibleGreater (Nothing,_) = True
        possibleGreater (x,y) = x > y
        possibleLess (Nothing,_) = True
        possibleLess (x,y) = x < y

solveAunt ∷ (Aunt → Bool) → String → ℤ
solveAunt start = number ∘ head ∘ filter start ∘ map readAunt ∘ lines

part1 ∷ String → ℤ
part1 = solveAunt possibleAunt

part2 ∷ String → ℤ
part2 = solveAunt possibleAunt'

main ∷ IO ()
main = do
  input ← readFile "P16.txt"
  putRender $ text "Advent of Code Problem 16"
          $+$ text "Part 1:" <+> integer (part1 input)
          $+$ text "Part 2:" <+> integer (part2 input)
