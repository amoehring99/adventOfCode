import Data.Char (isDigit)

split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = []
split delim str =
  let (before, remainder) = span (/= delim) str
   in before : case remainder of
        [] -> []
        x -> split delim (tail x)

contains :: (Eq a) => a -> [a] -> Bool
contains _ [] = False
contains x (y : ys) = x == y || contains x ys

splitGames :: String -> [String]
splitGames = split '\n'

splitID :: String -> [String]
splitID s = tail $ split ':' s

splitRounds :: String -> [String]
splitRounds = split ';'

splitColors :: String -> [String]
splitColors = split ' '

getAmount :: String -> Int
getAmount s = read (head (split ' ' s))

getColor :: String -> String
getColor s = head (tail (split ' ' s))

parseRGB :: (Num c) => String -> c -> (c, c, c) -> (c, c, c)
parseRGB "red" amount (r, g, b) = (r + amount, g, b)
parseRGB "green" amount (r, g, b) = (r, g + amount, b)
parseRGB "blue" amount (r, g, b) = (r, g, b + amount)

isPossible :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
isPossible (rBag, gBag, bBag) (r, g, b) = rBag >= r && gBag >= g && bBag >= b

countId :: (Int, [Bool]) -> Int
countId (id, possibilities) = if and possibilities then id else 0

main = do
  contents <- readFile "/home/amoehring99/git/adventOfCode/2023/2nd/puzzleInput.txt"
  let games = splitGames contents
  let ids = map splitID games
  let rounds = map splitRounds ids
  let colors = map splitColors rounds
  let amounts = map getAmount colors
  let colors = map getColor colors
  let rgb = foldr (uncurry parseRGB) (0, 0, 0) (zip colors amounts)
  let possible = map (isPossible rgb) (map (\x -> read x :: (Int, Int, Int)) (map (filter isDigit) colors))
  let count = map countId (zip [1 ..] possible)
  print (sum count)