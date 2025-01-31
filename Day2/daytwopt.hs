import Data.List

{- Remove all characters that aren't 0 to 9 or g,r,b or ; -}
removeExtra :: String -> String
removeExtra st = [ c | c <- st, c `elem` test]

test = ['g','r','b'] ++ ['0'..'9']

--  Remove r's that follow g's
removeR :: String -> String
removeR []                            = []
removeR (x:xs)
    |x /= 'g'                         = x : removeR xs
    |(x == 'g') || ((head xs) == 'r') = x : removeR (tail xs)

-- get second item in list
find' :: Int -> String -> Char
find' x a = head (drop (x - 1) a)

-- Format data into (a,b,c) tuple
formatData :: String -> [(Int,Char)]
formatData []                                               = []
formatData (x:xs)
    |xs == []                                               = []
  --  |x == ';'                                               = formatData xs
    |(x `elem` ['0'..'9']) && ((head xs) `elem` ['0'..'9']) = [ (read ([x] ++ [head xs]) :: Int, (find' 2 xs)) ] ++ formatData (drop 2 xs)
    |(x `elem` ['0'..'9'])                                  = [ (read [x] :: Int               ,  head xs   ) ] ++ formatData (drop 1 xs)
    |otherwise                                              = []

-- perform three steps above
clean xs = formatData( removeR( removeExtra ( dropWhile (/= ':') xs)))

-- filter all but highest number of each number
clean' :: [(Int,Char)] -> [(Int,Char)]
clean' []                       = []
clean' (ab:abs)
    |abs == []                  = (ab:abs)
    |(snd ab) /= (snd (head abs))      = head abs : clean' (ab : (tail abs))
    |(snd ab) == (snd (head abs))      = if (fst ab) >= (fst (head abs)) then  clean' (ab : (tail abs)) else clean' abs

-- 
clean'' :: [(Int,Char)] -> [(Int,Char)]
clean'' []      = []
clean'' (abs)
    |abs == []  = []
    |otherwise  = last (clean' abs) : (clean'' (init (clean' (abs))))

-- sum firth argument in tuple
prodTuple :: [(Int,Char)] -> Int
prodTuple (abs) = product (fst (unzip abs))

-- compare data against input value
compare' :: (Int,Char) -> (Int,Char) -> Bool
compare' (a,b) (c,d)
    |b /= d             = True
    |b == d             = if a >= c then True else False

-- Create a function that maps a function with two arguments over a list
mapF f a b = map (`f` b) a

-- map compare function to given input
mapComp :: [(Int,Char)] -> (Int,Char) -> [Bool]
mapComp a b = mapF compare' a b      

-- Compare all input values to hole line
mapComp' :: [(Int,Char)] -> [(Int,Char)] -> [[Bool]]
mapComp' a b = map (mapComp a) b

-- look for any fails in pattern
check :: [Bool] -> Bool 
check (x:xs)
    |xs == []           = x
    |otherwise          = (x && (head xs)) && check xs

-- control values
values = [(12,'r'),(13,'g'),(14,'b')]

-- format the data and check it agains the test valies
check' :: String -> Bool
check' xs = check (concat (mapComp' values (clean xs)))

add' :: [Bool] -> [Int]
add' xs = add'' (zip xs [1,2..])

add'' :: [(Bool,Int)] -> [Int]
add'' []         = []
add'' ((x,y):xys)
    |x          = [y] ++ add'' xys
    |otherwise  = add'' xys

-- Main program body --
main :: IO ()

{- define a function that recieves a file and outputs an Int -}
getInts :: FilePath -> IO ()
{- Convert file into a list of strings and perform the decoding process -}
getInts path = do
    contents <- readFile path
    let someLines = lines contents
    print someLines
    let answer = map clean someLines
    print answer
    let answer' = map clean'' answer
    print answer'
    let answer'' = map prodTuple answer'
    print answer''
    -- let boolcheck = map check' someLines
    -- print boolcheck
    -- let valboolzip = zip boolcheck answer''
    -- print valboolzip
    let answerfinal = sum answer''


{- print answer to sreen and return value -}
    print answerfinal
    return ()
-- ask for a file name to decode then perform function
main = do
    putStr "File: "
    file <- getLine
    getInts file
    return()