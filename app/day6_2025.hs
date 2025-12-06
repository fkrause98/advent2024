-- |

module Main where

import Utils
import Data.List (groupBy, group, sortOn, transpose)
import Data.Function (on)


resultFor :: String -> [Int] -> Int
resultFor "*" xs = product xs
resultFor "+" xs = sum xs

part1 input =
  let flat =  concatMap (zip [0..]) $ reverse input
      g = groupBy ((==) `on` fst) $ sortOn fst flat
      results = map ( \col ->
                      let
                        [(_, op)] = take 1 $ col
                        xs = map (\(_, num) -> read num :: Int) $ drop 1 col
                      in
                        resultFor op xs
                      ) g
  in
    sum results


readNums :: [[String]] -> [[Int]]
readNums s = map (map read) s

doMath :: ([Int], String) -> Int
doMath (inputs, "+") = sum inputs
doMath (inputs, "*") = product inputs

part2 xs = sum $ map doMath xs

main :: IO ()
main = do
  input <- actualInput
  let ops = words $ last $ lines $ input
  let inputs = splitOnWhitespace $ transpose $ init $ lines $ input
  print $ part2 $ zip (readNums inputs) ops

testInput = readFile  "./input/day6_2025_test.txt"

actualInput = readFile "./input/day6_2025.txt"
