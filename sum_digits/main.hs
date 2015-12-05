import Control.Applicative
import System.Environment (getArgs)

digits :: Int -> [Int]
digits 0 = []
digits n =
  let (q, r) = quotRem n 10
  in  r : digits q

main :: IO ()
main = do
  f <- head <$> getArgs
  input <- lines <$> readFile f
  mapM_ putStrLn (map (filter ((/=) '"') . show . sum . digits . read) input)
