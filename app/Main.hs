module Main where
import Data.List
import Data.Bifunctor

part1 :: IO ()
part1 = do
  input <- readFile "./input/day1.txt"
  let (list1, list2) = createAndSortLists $ inputAsTuples input
  let distances = calcDistances list1 list2
  print $ sum distances

main :: IO ()
main = do
  input <- readFile "./input/day1.txt"
  let (list1, list2) = createAndSortLists $ inputAsTuples input
  let appearances = map (`calcAppearances` list2)  list1
  let scores = zipWith (*) list1 appearances
  print (sum scores)


calcAppearances :: Integer -> [Integer] -> Integer
calcAppearances _ [] = 0
calcAppearances x (y:ys) = if x == y then 1 + calcAppearances x ys else calcAppearances x ys

calcDistances :: [Integer] -> [Integer] -> [Integer]
calcDistances = zipWith (\x y -> abs (x - y))

createAndSortLists :: [(Integer, Integer)] -> ([Integer], [Integer])
createAndSortLists tuplesInput = bimap sort sort (unzip tuplesInput)

inputAsTuples :: String -> [(Integer, Integer)]
inputAsTuples input = map lineToTuple (lines input)

lineToTuple :: String -> (Integer, Integer)
lineToTuple line = case words line of
  [elem1, elem2] -> (read elem1, read elem2)
  _ -> error "Wrong input"
