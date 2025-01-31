import Data.Char (isDigit)
import Data.List (last)
import Distribution.Compat.CharParsing (CharParsing(string))

nonDigit :: Char -> Bool
nonDigit = not . isDigit

extractNumbers :: String -> [Int]
extractNumbers [] = []
extractNumbers xs = do
    let afterDroppingNonDigits = dropWhile nonDigit xs
    let num = takeWhile isDigit afterDroppingNonDigits
    let afterDroppingNumber = dropWhile isDigit afterDroppingNonDigits
    if num == [] then []
    else [read num :: Int] ++ extractNumbers afterDroppingNumber

digits :: Int -> [Int]
digits n = map (\x -> read [x] :: Int) (show n)

calculate :: [Int] -> Int
calculate xs = do
    let finalNumber = last xs
    let firstNumber = head xs
    let firstNumberParsed = head (digits firstNumber)
    let finalNumberParsed = last (digits finalNumber)
    stringToInt ((show firstNumberParsed ) ++ (show finalNumberParsed))


stringToInt :: String -> Int
stringToInt s = read s

main :: IO ()
getInts :: FilePath -> IO Int
getInts path = do
    contents <- readFile path
    let someLines = lines contents
    let extractedNum = map extractNumbers someLines
    print $ extractedNum
    let calculatedNum = map calculate extractedNum
    print calculatedNum
    
    let answer = sum calculatedNum
    print answer
    return answer

main = do
  getInts "input.txt"
  return()
