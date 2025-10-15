
{-# LANGUAGE DataKinds #-}
module Main where

import Data.Matrix
import Data.List (findIndex)
import Data.Set (insert)
import qualified Data.Set as Set
import qualified Debug.Trace


main :: IO ()
main = part1

part1 :: IO ()
part1 = do
  (charMatrix, guardState) <- buildMap <$> readFile "./input/day6.txt"
  print charMatrix
  print guardState
  let visited = walk guardState charMatrix (Set.singleton (position guardState))
  print visited
  print $ markMap charMatrix visited



data Direction = North | South | West | East
  deriving (Eq, Show)
data GuardState = GuardState { position :: (Int, Int), dir :: Direction } deriving (Eq, Show)
type Visited = Set.Set (Int, Int)

turnRight :: GuardState -> Direction
turnRight GuardState { dir = North } = East
turnRight GuardState { dir = South } = West
turnRight GuardState { dir = West } = North
turnRight GuardState { dir = East } = South

increment :: GuardState -> (Int, Int)
increment GuardState {dir = North} = (-1, 0)
increment GuardState {dir = South} = (1, 0)
increment GuardState {dir = West} = (0, -1)
increment GuardState {dir = East} = (0, 1)

buildMap :: String -> (Matrix Char, GuardState)
buildMap rawMap = (fromLists mmap, GuardState { position = (guardRow + 1, guardColumn + 1), dir = North })
  where
    mmap = lines rawMap
    guardRow = findIndex' (\lst -> elem '^' lst) mmap
    guardColumn = findIndex' (\c -> c == '^') (mmap !! guardRow)

findIndex' :: (a -> Bool) -> [a] -> Int
findIndex' f lst = case findIndex f lst of
  Just indx -> indx
  Nothing -> error "Could not find element"

walk :: GuardState -> Matrix Char -> Visited -> Visited
walk guard mmap visited =
    case safeGet newX newY mmap of
      Just '#' -> walk ( guard { dir = turnRight guard  } ) mmap visited
      Just _ -> walk (guard { position = (newX, newY) }) mmap (insert (currX, currY) visited)
      Nothing -> (insert (currX, currY) visited)
  where
    (x, y) = (increment guard)
    (currX, currY) = position guard
    (newX, newY) = (currX + x, currY + y)

markMap  :: Matrix Char -> Visited -> Matrix Char
markMap mmap visited =
    mapPos
      ( \(x, y) char ->
          if Set.member (x, y) visited  then
           'X'
           else
            char
      )
      mmap
