module Main where
import Data.List
import Data.Bifunctor

part1 :: IO ()
part1 = do
  input <- readFile "./input/day2.txt"
  let levels = map words (lines input)
  let levelsInt = map (\lst -> (map (read) lst) :: [Integer]) levels
  let safeLevels = map isLevelSafe levelsInt
  let asNums = foldl (\total isSafeLevel -> if isSafeLevel then 1 + total else total) 0 safeLevels
  print asNums



allIncreasing :: [Integer] -> Bool
allIncreasing (x:y:ys) =
  x > y && allIncreasing (y:ys)
allIncreasing xs = True

allDecreasing :: [Integer] -> Bool
allDecreasing (x:y:ys) =
  x < y && allDecreasing (y:ys)
allDecreasing xs = True

diffsInRange :: [Integer] -> Bool
diffsInRange (x:y:ys) =
  (abs (x-y) >= 1) && (abs (x-y) <= 3) && diffsInRange (y:ys)
diffsInRange xs = True

isLevelSafe :: [Integer] -> Bool
isLevelSafe lst = (allIncreasing lst || allDecreasing lst) && diffsInRange lst

-- part2 :: IO ()
-- part2 = do
--   input <- readFile "./input/day1.txt"
--   let (list1, list2) = createAndSortLists $ inputAsTuples input
--   let appearances = map (`calcAppearances` list2)  list1
--   let scores = zipWith (*) list1 appearances
--   putStrLn $ "Part 2 solution: " ++ show (sum scores)

main :: IO ()
main = do
  part1
  -- part2


