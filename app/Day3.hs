module Main where
import Data.Bifunctor()
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.Void
type Parser = Parsec Void String

leftParen :: Parser Char
leftParen = char '('
rightParen :: Parser Char
rightParen = char ')'
comma :: Parser Char
comma = char ','
mul :: Parser Char
mul = do
  _ <- char 'm'
  _ <- char 'u'
  char 'l'
mulInstruction :: Parser (Integer, Integer)
mulInstruction = do
  _ <- mul
  _ <- leftParen
  x <- decimal
  _ <- comma
  y <- decimal
  _ <- rightParen
  return (x, y)

extract :: Parser a -> Parser [a]
extract p = do (:) <$> try p <*> extract p
        <|> do anySingle >> extract p
        <|> do eof >> return []

getResult :: [(Integer, Integer)] -> Integer
getResult = sum . (map  mulTuple)
  where
    mulTuple (x, y) = x * y

part1 :: IO ()
part1 = do
  input <- readFile "./input/day3.txt"
  let matches = parse (extract mulInstruction) "" input
  case matches of
    Left err -> print $ errorBundlePretty err
    Right results -> print $ "Part 1 result: " ++ show (getResult results)


-- part2 :: IO ()
-- part2 = do
--   input <- readFile "./input/day2.txt"
--   let levels = map words (lines input)
--   let levelsInt = map (\lst -> (map (read) lst) :: [Integer]) levels
--   let safeLevels = map isLevelSafe2 levelsInt
--   let asNums = foldl (\total isSafeLevel -> if isSafeLevel then 1 + total else total) 0 safeLevels
--   putStrLn $ "Part 2: " ++ show asNums

main :: IO ()
main = do
  part1
  -- part2
