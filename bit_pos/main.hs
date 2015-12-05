import qualified Data.Text as T
import System.Environment (getArgs)

flags :: Int -> [Int]
flags 0 = []
flags n =
  let (quotient, remainder) = quotRem n 2
  in remainder : flags quotient

handleMath :: Int -> Int -> Int -> Bool
handleMath x b1 b2
  | b1 == b2 = True
  | b1 < 1 || b2 < 1 = False
  | 2 ^ b1 > x = False
  | 2 ^ b2 > x = False
  | otherwise =
    let xFlags = flags x
        f1 = head $ drop (b1 - 1) xFlags
        f2 = head $ drop (b2 - 1) xFlags
    in  f1 == f2

handle :: String -> String
handle s =
  let [x,p1,p2] = map (read . T.unpack) $ T.splitOn (T.pack ",") (T.pack s)
  in case handleMath x p1 p2 of
      True -> "true"
      False -> "false"

main = do
  [file] <- getArgs
  input <- readFile file
  mapM_ (putStrLn . handle) $ lines input
