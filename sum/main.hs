import Control.Applicative
import System.Environment (getArgs)

main :: IO ()
main = do
  f <- head <$> getArgs
  ls <- lines <$> readFile f
  let nums = map read ls :: [Int]
  putStrLn $ show $ sum nums
