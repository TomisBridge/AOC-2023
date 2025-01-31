checkNotZero :: [Int] -> Int 
checkNotZero []         = 0
checkNotZero (x:xs) = if x /= 0 then x else checkNotZero xs