range' :: Int -> [Int]
range' 0 = []
range' n = n : (range' $ n - 1)

range :: Int -> [Int]
range = reverse . range'

all :: [Bool] -> Bool
all = foldl (&&) True

prime :: Int -> Bool
prime int =
  let denoms = filter (flip (>) 1) $ range $ int - 1
      divisors = filter ((==) 0 . (mod) int) denoms
  in
      0 == length divisors

palindrome :: Int -> Bool
palindrome int =
  let digits = show int
  in  case digits of
        [a,b,c] -> a == c
        [a,b] -> a == b
        [a] -> True
        otherwise -> False

main = do
  putStrLn . show . maximum . (filter (\a -> prime a && palindrome a)) $ range 999
