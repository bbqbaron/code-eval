range :: Int -> [Int]
range = reverse . range' where
  range' 0 = []
  range' n = n : sub where
    sub = range' $ n - 1

addPrime :: Int -> Int -> Int -> Int
addPrime current thusFar total = f current thusFar total where
    f current 1000 total = total
    f current thusFar total =
        let isPrime = prime current
            toAdd = if isPrime then current else 0
            total' = toAdd + total
            current' = current + 1
            thusFar' = thusFar + (if isPrime then 1 else 0)
        in
            addPrime current' thusFar' total'

prime :: Int -> Bool
prime 0 = False
prime 1 = False
prime 2 = True
prime int =
  let denoms = filter (flip (>) 1) $ range $ int - 1
      divisors = filter ((==) 0 . (mod) int) denoms
  in
      0 == length divisors

main :: IO ()
main = do
  putStrLn . show $ addPrime 0 0 0
