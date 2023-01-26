isReplica :: String -> Int -> Char -> Bool
isReplica [] 0 ch = True
isReplica [] n ch = False
isReplica xs 0 ch = False
isReplica (x:xs) n ch | [x | x <- xs, x /= ch] == [] && length [x | x <- xs, x == ch] + 1 == n && x == ch = True
                      | otherwise = False

main = do
    a <- getLine
    b <- getLine
    c <- getChar
    let result = isReplica a (read b) c
    print result
