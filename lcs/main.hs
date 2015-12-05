import Control.Applicative
import Data.List
import Data.Ord
import System.Environment (getArgs)

splitOn' :: [String] -> String -> Char -> String -> [String]
splitOn' acc buf _ "" = buf : acc
splitOn' acc buf sep (h:t) =
  let hit = h == sep
  in
    splitOn'
      (if hit then buf : acc else acc)
      (if hit then "" else h : buf)
      sep
      t

splitOn :: Char -> String -> [String]
splitOn sep s = splitOn' [] "" sep s

subseq :: String -> [String]
subseq "" = []
subseq [a] = [[a], []]
subseq [a,b] = [[a,b], [a], [b], []]
subseq [a,b,c] = [[a,b,c], [a,b], [a,c], [b,c], [a], [b], [c], []]
subseq (h:t) = concat [map ((:) h) sub, sub]
  where sub = subseq t

head' :: [String] -> String
head' [] = ""
head' (a:_) = a

getLcs :: [String] -> String
getLcs [a,b] = head' $ reverse $ sortBy (comparing length) $ intersect (subseq a) (subseq b)
getLcs l = ""

handle :: String -> String
handle = getLcs . (take 2) . (map reverse) . (splitOn ';')

main :: IO ()
main = do
  f <- head <$> getArgs
  ls <- lines <$> readFile f
  let results = map handle $ filter (/= "") ls
  mapM_ putStrLn results
