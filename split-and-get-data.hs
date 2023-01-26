wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

filterByPosition :: [String] -> [String]
filterByPosition [] = []
filterByPosition (x:y:z:xs) = z : filterByPosition xs


stringToDouble :: [String] -> [Double]
stringToDouble [] = []
stringToDouble (x:xs) = (read x :: Double) : stringToDouble xs

maximumm :: [Double] -> Double
maximumm [] = 0
maximumm (x:xs) | maximumm xs > x = maximumm xs
                | otherwise = x

minimumm :: [Double] -> Double
minimumm [] = 0
minimumm (x:xs) | minimum xs < x = minimumm xs
               | otherwise = x

maxMin :: [Double] -> (Double, Double)
maxMin [] = (0, 0)
maxMin (x:xs) = (minimumm (x:xs), maximumm (x:xs))

main = do
    a <- getLine
    let result = maxMin (stringToDouble (filterByPosition (wordsWhen (==';') a)))
    print result
