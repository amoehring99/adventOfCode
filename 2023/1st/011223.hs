import Data.Char (isDigit)
import Data.Text (Text, pack, replace, unpack)

concatFirstAndLastDigit :: String -> String
concatFirstAndLastDigit [] = []
concatFirstAndLastDigit [x] = x : [x]
concatFirstAndLastDigit (x : xs) = x : [last xs]

sumOfDigits :: [String] -> Int
sumOfDigits = foldr ((+) . read) 0

replaceWrittenDigitWithDigit :: [String] -> [String]
replaceWrittenDigitWithDigit =
  map
    ( unpack
        . replace (pack "zero") (pack "z0o")
        . replace (pack "one") (pack "o1e")
        . replace (pack "two") (pack "t2o")
        . replace (pack "three") (pack "t3e")
        . replace (pack "four") (pack "f4r")
        . replace (pack "five") (pack "f5e")
        . replace (pack "six") (pack "s6x")
        . replace (pack "seven") (pack "s7n")
        . replace (pack "eight") (pack "e8t")
        . replace (pack "nine") (pack "n9e")
        . pack
    )

main = do
  contents <- readFile "/home/amoehring99/git/adventOfCode/2023/1st/puzzleInput.txt"
  print . sumOfDigits . map (concatFirstAndLastDigit . filter isDigit) . replaceWrittenDigitWithDigit . words $ contents