import Control.Applicative
import System.Environment (getArgs)

pretty :: (Show a) => a -> String
pretty = filter ((/=) '"') . show

nFib :: Int -> Int
nFib 1 = 1
nFib 2 = 1
nFib n
  | n < 0 = 0
  | otherwise = sum $ map (\x -> nFib (n - x)) [1,2]

main = do
  f <- head <$> getArgs :: IO FilePath
  ls <- lines <$> readFile f
  mapM_ putStrLn (map (pretty . nFib . read) ls)
