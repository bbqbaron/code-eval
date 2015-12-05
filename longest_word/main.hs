import Control.Applicative
import System.Environment (getArgs)

pretty :: (Show a) => a -> String
pretty = filter ((/=) '"') . show

handle :: String -> String
handle line =
  let ws = words line
      maxLength = maximum $ map length ws
      qualifying = filter ((==) maxLength . length) ws
  in  case qualifying of
        [] -> ""
        (x:_) -> x

main :: IO ()
main = do
  f <- head <$> getArgs
  ls <- lines <$> readFile f
  mapM_ putStrLn $ map (pretty . handle) ls
