import qualified Data.Set as S
import System.Environment (getArgs)

digits :: Int -> [Int]
digits 0 = []
digits n =
  let (q, r) = quotRem n 10
  in  r : digits q

replace :: Int -> Int
replace n = sum $ map (flip (^) 2) $ digits n

f :: (S.Set Int) -> Int -> Bool
f _ 1 = True
f seen n
  | S.member n seen = False
  | otherwise = f (S.insert n seen) (replace n)

handleLine :: Int -> Bool
handleLine 1 = True
handleLine n = f S.empty n

main = do
  [file] <- getArgs
  input <- readFile file
  let output = map (handleLine . read) $ lines input
  mapM_ (putStrLn . (\a -> if a then "1" else "0")) output
