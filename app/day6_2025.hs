{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import Data.Function (on)
import Data.List (group, groupBy, sortOn, transpose)
import Utils

resultFor :: String -> [Int] -> Int
resultFor "*" xs = product xs
resultFor "+" xs = sum xs

part1 input =
    let
        flat = concatMap (zip [0 ..]) $ reverse input
        g = groupBy ((==) `on` fst) $ sortOn fst flat
        results =
            map
                ( \col ->
                    let
                        [(_, op)] = take 1 $ col
                        xs = map (\(_, num) -> read num :: Int) $ drop 1 col
                     in
                        resultFor op xs
                )
                g
     in
        sum results

doMath :: ([Int], String) -> Int
doMath (inputs, "+") = sum inputs
doMath (inputs, "*") = product inputs

part2 :: String -> Int
part2 input = sum $ map doMath mathProblems
  where
    ops = words $ last $ lines $ input
    inputs = splitOnWhitespace $ transpose $ init $ lines $ input
    mathProblems = zip (readNums inputs) ops

main :: IO ()
main = do
    input <- actualInput
    print $ part2 input

testInput = readFile "./input/day6_2025_test.txt"

actualInput = readFile "./input/day6_2025.txt"
