concatLista :: [Int] -> Int
concatLista [] = 0
concatLista (x:xs) = x*10^(length (x:xs) - 1) + concatLista xs

digs :: Int -> [Int]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

somarListas :: [Int] -> [Int] -> [Int]
somarListas [] [] = []
somarListas (x:xs) [] = (x:xs)
somarListas [] (s:ss) = (s:ss)
somarListas (x:xs) (s:ss) = digs (concatLista (x:xs) + concatLista (s:ss))


main :: IO ()
main = do
input1 <- getLine
input2 <- getLine
let result = somarListas (read input1 :: [Int])  (read input2 :: [Int])
print result
