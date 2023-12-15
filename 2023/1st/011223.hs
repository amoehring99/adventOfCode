import Data.Char (isDigit)

concatFirstAndLastDigit :: String -> String
concatFirstAndLastDigit [] = []
concatFirstAndLastDigit [x] = x : [x]
concatFirstAndLastDigit (x : xs) = x : [last xs]

sumOfDigits :: [String] -> Int
sumOfDigits = foldr ((+) . read) 0

main = do
  contents <- readFile "/home/amoehring99/git/adventOfCode/2023/1st/puzzleInput.txt"
  print . sumOfDigits . map (concatFirstAndLastDigit . filter isDigit) . words $ contents