wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'


checkInclude :: [String] -> String -> [String]
checkInclude [] _ = []
checkInclude (x:y:z:xs) a | a `elem` words x = z : checkInclude xs a
                          | otherwise = checkInclude xs a


stringToDouble :: [String] -> [Double]
stringToDouble [] = []
stringToDouble (x:xs) = (read x :: Double) : stringToDouble xs

sumAll :: [Double] -> Double
sumAll [] = 0
sumAll (x:xs) = foldl (\a b -> a + b) x xs

main = do
    a <- getLine
    b <- getLine
    let result = sumAll (stringToDouble (checkInclude (wordsWhen (==';') b) a))
    print result
