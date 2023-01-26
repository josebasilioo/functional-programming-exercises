sumTo :: Int -> Int
sumTo 0 = 0
sumTo n
  | n > 0 = n + sumTo (n -1)
  | otherwise = n

main :: IO ()
main = interact $ show . sumTo . read
