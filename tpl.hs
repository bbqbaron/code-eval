import Control.Applicative
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

pretty :: (Show a) => a -> String
pretty = filter ((/=) '"') . show

handle :: String -> String
handle s = s

main :: IO ()
main = do
  f <- head <$> getArgs
  ls <- lines <$> readFile f
  let results = map handle ls
  mapM_ putStrLn results
