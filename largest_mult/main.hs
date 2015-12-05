import qualified Data.Text as T
import System.Environment (getArgs)

handleMath :: Int -> Int -> Int -> Int
handleMath x n' n
  | x <= n = n
  | x > n = handleMath x n' (n+n')

handle :: String -> String
handle s =
  let [x,n] = map (read . T.unpack) $ T.splitOn (T.pack ",") (T.pack s)
  in filter ((/=) '"') . show $ handleMath x n n

main = do
  [file] <- getArgs
  input <- readFile file
  mapM_ (putStrLn . handle) $ lines input
