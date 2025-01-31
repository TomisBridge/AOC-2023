import Data.List

{- Remove all characters that aren't 0 to 9 -}
removeChar :: String -> String
removeChar st = [ c | c <- st, c `elem` ['0'..'9']]

{- Combine the first and last of a string of characters and return them as an Int -}
-- finNum added for part 2 of problem
decode :: String -> Int
decode xs = decode' (removeChar (findNum xs))

decode' :: String -> Int
decode' xs = read ([head xs] ++ [last xs]) :: Int

{- Make a Tuple of spelt number and there corresponding characters -}
numSpell = [('1',"one"),('2',"two"),('3',"three"),('4',"four"),('5',"five"),('6',"six"),('7',"seven"),('8',"eight"),('9',"nine")]

-- Create a function that maps a function with two arguments over a list
mapF f a b = map (`f` a) b

-- Map the num tupler to a String and output character if its a prefix
mapPref' :: (Char,String) -> String -> Char
mapPref' (a,b) (c:cs)
    | b `isPrefixOf` (c:cs) = a
    | otherwise        = c

-- replace all sequential duplicates in a list
compress :: Eq a => [a] -> [a]
compress (x:xs)
    | xs == []        = [x]
    | x == (head xs) = compress xs
    | otherwise      = x : compress xs

-- map prefix check function over numbers 1 to 9 and compress the result
mapPrefix a = compress (mapF mapPref' a numSpell)

-- Interate the over the input and insert characters if prefix exist
findNum :: String -> String
findNum []     = []
findNum (x:xs) = mapPrefix(x:xs) ++ findNum xs


-- Main program body --
main :: IO ()

{- define a function that recieves a file and outputs an Int -}
getInts :: FilePath -> IO Int

{- Convert file into a list of strings and perform the decoding process -}
getInts path = do
    contents <- readFile path
    print contents
    let someLines = lines contents
    print someLines
    let answer = sum (map decode someLines)

{- print answer to sreen and return value -}
    print answer
    return answer

-- ask for a file name to decode then perform function
main = do
    putStr "File: "
    file <- getLine
    getInts file
    return()