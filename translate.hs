searchValue :: [(Char, Char)] -> Char -> Char
searchValue [] _ = '\0'
searchValue ((a,b):xs) v | a == v = b
                         | otherwise = searchValue xs v

readLetters :: String -> [(Char, Char)] -> String
readLetters [] [] = []
readLetters [] _ = []
readLetters _ [] = []
readLetters (x:xs) (y:ys) = searchValue (y:ys) x : readLetters xs (y:ys)

main = do
    a <- getLine
    b <- getLine
    let result = readLetters a (read b)
    print result
