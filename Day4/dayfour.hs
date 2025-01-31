import Data.List

intChar = ['0'..'9']

-- halve a list
halve :: [Int] -> ([Int],[Int])
halve xs = ((take 5 xs),(drop 5 xs))

-- strip characters from data set
stripChar :: String -> [Int]
stripChar []        = []
stripChar [x]
    | any (== x) intChar = [read [x] :: Int]
    | otherwise          = []
stripChar (x:xs)
    | any (x ==) intChar = if any ((head xs) ==) intChar then (read (x : [head xs]) :: Int) : (stripChar (tail xs))
                                                         else (read [x] :: Int) : stripChar xs
    | otherwise          = stripChar xs

-- strip first character then halve lists
stripChar' :: String -> ([Int],[Int])
stripChar' xs = halve( stripChar xs)

-- mech the number of matches the right side has with the left for each car
rlMatch :: ([Int],[Int]) -> Int
rlMatch ([],_)          = 0
rlMatch (_,[])          = 0
rlMatch ((x:xs),ys)
    | any (== x) ys     = 1 + rlMatch (xs,ys)
    | otherwise         = rlMatch (xs,ys)

-- add 0,Int to list of matches
formatZero :: [Int] -> [(Int,Int)]
formatZero []       = []
formatZero (x:xs)   = (0,x) : formatZero xs

-- incriment 1 to first counter
firstOne :: [(Int,Int)] -> [(Int,Int)]
firstOne ((a,b):xs) = (1,b) : xs

-- incriment matches for part 2
incMatch :: [(Int,Int)] -> [(Int,Int)]
incMatch (x:xs) 

-- convert number of matches into points
score :: Int -> Int
score 0 = 0
score 1 = 1
score x = 2 * score (x - 1)

-- Main program body --
main :: IO ()

{- define a function that recieves a file and outputs an Int -}
getInts :: FilePath -> IO ()

{- Convert file into a list of strings and perform the decoding process -}
getInts path = do
    contents <- readFile path
    --print contents
    let someLines = lines contents
    let someLines' = map (dropWhile (/= ':')) someLines
    print someLines'
    let stripLines = map stripChar' someLines'
    putStrLn "stipLines"
    print stripLines
    let matches = map rlMatch stripLines
    putStrLn "matches"
    print matches
    let scores = map score matches
    putStrLn "scores"
    print scores
    let points = sum scores
    putStrLn "points"
    print points


-- ask for a file name to decode then perform function
main = do
--    putStr "File: "
--   file <- getLine
    getInts "test.txt"
    return()