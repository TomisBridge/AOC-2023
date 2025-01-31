import Data.Matrix
import Data.Maybe
import Data.List

-- look for any fails in pattern
checkBool :: [Bool] -> Bool 
checkBool (x:xs)
    |xs == []           = x
    |otherwise          = (x || (head xs)) || checkBool xs

-- List of specail character to check for
specialCharacters = ['!'..'-']++['/']++[':'..'?']++['['..'_']++['{'..'~']++['@','`']

-- matrix to test function
testMatrix = fromLists ["467..114..","...*......","..35..633.","......#...","617*......",".....+.58.","..592.....","......755.","...$.*....",".664.598.."]

-- Check a character against list of special characters
mapSpecialK :: Char -> Bool
mapSpecialK a = checkBool (map (a ==) specialCharacters)

-- attach a 1 or 0 to a character if it is a number
numCheck :: Char -> Bool
numCheck c
    | [c] `isInfixOf` ['0'..'9'] = True
    | otherwise                  = False


safeGetAdj :: (Int,Int) -> Matrix Char -> [Bool]
safeGetAdj ijs m = map (`safeGetAdj'` m) xys
                    where xys = seroundList ijs

-- Output 1 for specail character or 0 for everything else
safeGetAdj' :: (Int,Int) -> Matrix Char -> Bool
safeGetAdj' (i,j) m
    | isNothing (safeGet i j m)             = False
    | mapSpecialK (getElem i j m)           = True
    | otherwise                             = False

-- list of positions serounding a given position
seroundList (i,j) = [ (x,y) | x <- map (i +) [-1..1], y <- map (j+) [-1..1]]

--
testMap m = mapPos (\(r,c) a ->((getElem r c m) , (checkBool (safeGetAdj (r,c) m)))) m

--
sumAdj :: [(Char,Bool)] -> [[(Char,Bool)]]
sumAdj abs
    | abs == []                            = []
    | not (numCheck (fst (head abs)))      = sumAdj (tail abs)
    | numCheck (fst (head abs))            = [sumAdj' abs] ++ sumAdj (drop (length (sumAdj' abs)) abs)

--
sumAdj' :: [(Char,Bool)] -> [(Char,Bool)]
sumAdj' (ab:abs)
    | (numCheck (fst ab)) == False  = []
    | numCheck (fst ab)             = [ab] ++ sumAdj' abs

--
takeTrue :: (String,[Bool]) -> Int
takeTrue (as,bs) = if checkBool bs then read as else 0

-- Main program body --
main :: IO ()

{- define a function that recieves a file and outputs () -}
getInts :: FilePath -> IO ()

{- Convert file into a list of strings and perform the decoding process -}
getInts path = do
    contents <- readFile path
    --print contents
    let someLines = lines contents
    print someLines
    putStrLn "<>"
    let matrix = {-extendTo '.' 140 141-} (fromLists someLines)
    print matrix
    putStrLn "<>"
    let boolMatrix = testMap matrix
    print boolMatrix
    putStrLn "sumMatrix"
    let sumMatrix = sumAdj (toList boolMatrix)                      {-*** map getRow instead of toList ***-}
    print sumMatrix
    putStrLn "unZipMatrix"
    let unZipMatrix = map unzip sumMatrix
    print unZipMatrix
    putStrLn "<>"
    let finalList = map takeTrue unZipMatrix
    print finalList
    putStrLn "<>"
    let answer = sum finalList
    print answer
    --let spch = [ c | c <- someLines, c `elem` ['0'..'9']]
    --print spch
-- ask for a file name to decode then perform function
main = do
    putStr "File: "
    file <- getLine
    getInts file
    return()