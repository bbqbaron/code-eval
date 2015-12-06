import Control.Applicative
import System.Environment (getArgs)

import Debug.Trace

data Dir = Up | Down

f :: Int -> Int -> Int -> Dir -> [Int] -> Int
f ps _ _ _ [] = ps * 10
f ps _ _ Down [i] = ps * 10
f ps _ l Up [i]
  | i < l = (ps + 1) * 10
  | otherwise = ps * 10
f ps lp l Up [i, i2]
  | i < l = f (ps + 1) l i Down [i2]
  | otherwise = f ps lp i Up [i2]
f ps lastPeak l Down [i, i2]
  | i > l && (abs (lastPeak - l) >= 20) = f ps lastPeak i Up [i2]
  | otherwise = f ps lastPeak i Down [i2]
f ps lp l Up (h:t)
  | h < l = f (ps + 1) l h Down t
  | otherwise = f ps lp h Up t
f ps lastPeak l Down (h:t)
  | h > l && (abs (lastPeak - l) >= 20) = f ps lastPeak h Up t
  | otherwise = f ps lastPeak h Down t

pretty :: (Show a) => a -> String
pretty = filter ((/=) '"') . show

handle :: String -> String
handle = pretty . (f 0 0 0 Down) . (map read) . words

main :: IO ()
main = do
  f <- head <$> getArgs
  ls <- lines <$> readFile f
  let results = map handle ls
  mapM_ putStrLn results
