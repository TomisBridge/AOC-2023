import Data.List

type Scratchcard = ([Int], [Int])

-- Function to calculate the score for a single scratchcard
calculateScore :: Scratchcard -> Int
calculateScore (winningNumbers, playerNumbers) = go playerNumbers winningNumbers 1
  where
    go :: [Int] -> [Int] -> Int -> Int
    go [] _ score = score
    go (x:xs) wn score
      | x `elem` wn = go xs wn (score * 2)
      | otherwise   = go xs wn score

-- Function to calculate the total score for a list of scratchcards
totalScore :: [Scratchcard] -> Int
totalScore = sum . map calculateScore

-- Example scratchcards
exampleScratchcards :: [Scratchcard]
exampleScratchcards =
  [ ([41, 48, 83, 86, 17], [83, 86, 6, 31, 17, 9, 48, 53])
  , ([13, 32, 20, 16, 61], [61, 30, 68, 82, 17, 32, 24, 19])
  , ([1, 21, 53, 59, 44], [69, 82, 63, 72, 16, 21, 14, 1])
  , ([41, 92, 73, 84, 69], [59, 84, 76, 51, 58, 5, 54, 83])
  , ([87, 83, 26, 28, 32], [88, 30, 70, 12, 93, 22, 82, 36])
  , ([31, 18, 13, 56, 72], [74, 77, 10, 23, 35, 67, 36, 11])
  ]

main :: IO ()
main = do
  let score = totalScore exampleScratchcards
  putStrLn $ "Total Score: " ++ show score