import Control.Applicative
import System.Environment (getArgs)

pretty :: (Show a) => a -> String
pretty = filter ((/=) '"') . show

handle :: String -> Int
handle l =
  let ls = map read $ words l
  in lowestIndex ls $ uniques ls

count :: [Int] -> Int -> Int
count l i = foldl (\ n x -> if x == i then n + 1 else n) 0 l

uniques :: [Int] -> [Int]
uniques l = filter (\ x -> (count l x) == 1) l

oneIndexOf :: Int -> [Int] -> Int
oneIndexOf n l =
  fn' 1 l where
  fn' :: Int -> [Int] -> Int
  fn' _ [] = 0
  fn' x (h:t) =
    if h == n then x else fn' (x + 1) t

lowestIndex :: [Int] -> [Int] -> Int
lowestIndex [] _ = 0
lowestIndex _ [] = 0
lowestIndex l2 l1 =
  let lowest = foldl1 min l1
      index = oneIndexOf lowest l2
  in  index

main :: IO ()
main = do
  f <- head <$> getArgs
  ls <- lines <$> readFile f
  mapM_ putStrLn (map (pretty . handle) ls)
