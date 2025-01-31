import Data.Matrix
import Data.Maybe
import Data.List
import Data.List.Split
import Data.Sort

-- look for any fails in pattern
checkBool :: [Bool] -> Bool 
checkBool (x:xs)
    |xs == []           = x
    |otherwise          = (x || (head xs)) || checkBool xs

-- List of specail character to check for
specialCharacters = ['*']{-['!'..'-']++['/']++[':'..'?']++['['..'_']++['{'..'~']++['@','`']-}

--
testList = ["467..114..","...*......","..35..633.","......#...","617*......",".....+.58.","..592.....","......755.","...$.*....",".664.598.."]

-- matrix to test function
testMatrix = fromLists ["467..114..","...*......","..35..633.","......#...","617*......",".....+.58.","..592.....","......755.","...$.*....",".664.598.."]

-- Check a character against list of special characters
mapSpecialK :: (Char,Int) -> Bool
mapSpecialK (c,x) = (x /= 0)

-- attach a 1 or 0 to a character if it is a number
numCheck :: Char -> Bool
numCheck c
    | [c] `isInfixOf` ['0'..'9'] = True
    | otherwise                  = False

checkNotZero :: [Int] -> Int 
checkNotZero []     = 0
checkNotZero (x:xs) = if x /= 0 then x else checkNotZero xs

--
incrimentGear :: Int -> [Char] -> [(Char,Int)]
incrimentGear x []      = []
incrimentGear x (c:cs)
    | c /= '*'          = (c,0) : incrimentGear x cs
    | c == '*'          = (c,x) : incrimentGear (x + 1) cs

--
incrimentGear' :: Int -> Char -> (Char,Int)
incrimentGear' x c = if c == '*' then (c,x) else (c,0)

--
safeGetAdj :: (Int,Int) -> Matrix (Char,Int) -> [Int]
safeGetAdj ijs m = map (`safeGetAdj'` m) xys
                    where xys = seroundList ijs

-- Output 1 for specail character or 0 for everything else
safeGetAdj' :: (Int,Int) -> Matrix (Char,Int) -> Int
safeGetAdj' (i,j) m
    | isNothing (safeGet i j m)             = 0
    | mapSpecialK (getElem i j m)           = snd (getElem i j m)
    | otherwise                             = 0

-- list of positions serounding a given position
seroundList (i,j) = [ (x,y) | x <- map (i +) [-1..1], y <- map (j+) [-1..1]]

--
--testMap :: Matrix (Char,Int) -> Matrix (Char, Int)
testMap m = mapPos (\(r,c) a ->((fst(getElem r c m)) , (checkNotZero (safeGetAdj (r,c) m)))) m

--

sumAdj :: [(Char,Int)] -> [[(Char,Int)]]
sumAdj abs
    | abs == []                            = []
    | not (numCheck (fst (head abs)))      = sumAdj (tail abs)
    | numCheck (fst (head abs))            = [sumAdj' abs] ++ sumAdj (drop (length (sumAdj' abs)) abs)


--

sumAdj' :: [(Char,Int)] -> [(Char,Int)]
sumAdj' (ab:abs)
    | (numCheck (fst ab)) == False  = []
    | numCheck (fst ab)             = [ab] ++ sumAdj' abs


--
takeTrue :: (String,[Int]) -> Int
takeTrue (as,bs) = if (checkNotZero bs) /= 0 then read as else 0

--
concatInt :: [(String,[Int])] -> [(String,Int)]
concatInt []                    = []
concatInt (cx:cxs)
    | (checkNotZero (snd cx)) /= 0    = ((fst cx), (checkNotZero (snd cx))) : concatInt cxs
    | otherwise                 = concatInt cxs

--
joinNumbers :: [(String,Int)] -> [Int]
joinNumbers []                     = []
joinNumbers (cx:cxs)
    |cxs == []                     = [0]
    |(snd cx) == (snd (head cxs))  = [(read (fst cx) :: Int) * ((read (fst (head cxs))) :: Int)] ++ joinNumbers (tail cxs)
    |otherwise                     = joinNumbers cxs

-- Main program body --
main :: IO ()

{- define a function that recieves a file and outputs () -}
getInts :: FilePath -> IO ()

{- Convert file into a list of strings and perform the decoding process -}
getInts path = do
    contents <- readFile path
    --print contents
    let someLines = lines contents
    --print someLines
    let someLinesIndexed = chunksOf 140 (incrimentGear 1 (concat someLines))
    --print someLinesIndexed
    let matrix = extendTo ('.',0) 140 141 (fromLists someLinesIndexed)
    --print matrix
    let boolMatrix = testMap matrix
    --print boolMatrix
    let sumMatrix = sumAdj (toList boolMatrix)                      {-*** map getRow instead of toList ***-}
    --print sumMatrix
    --putStrLn "<unZip>"
    let unZipMatrix = map unzip sumMatrix
    --print unZipMatrix
    --putStrLn "<concatI>"
    let concatI = concatInt unZipMatrix
    --print concatI
    --putStrLn "<finalList>"
    let finalList = sortOn snd concatI
    --print finalList
    --putStrLn "<answer>"
    let answer = joinNumbers finalList
    --print answer
    let answer' = sum answer
    print answer'
-- ask for a file name to decode then perform function
main = do
--    putStr "File: "
--    file <- getLine
    getInts "code.txt"
    return()