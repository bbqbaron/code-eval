import Control.Applicative
import System.Environment (getArgs)

compress :: String -> String
compress = filter ((/=) ' ')

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

play :: Int -> Int -> Int -> [String] -> String
-- play 0 _ _ _ = ""
-- play _ _ _ [] = ""
play _ _ _ [p] = p
play len acc pos players =
  let newPos = if pos >= length players then 0 else pos + 1
      newPlayers = if acc == len then
        let (h, t) = splitAt pos players
        in concat h (tail t)
      else players
      newAcc = if acc == len then 0 else acc + 1
  in  play len newAcc newPos newPlayers

handle :: (Show a) => String -> a
handle s =
  let [playerString, numString] = splitOn '|' s
      players = map compress $ splitOn ',' playerString
      num = read . compress numString

main :: IO ()
main = do
  f <- head <$> getArgs
  ls <- lines <$> readFile f
  let results = map pretty . handle ls :: [String]
  in  mapM_ putStrLn results
