import System.Environment (getArgs)

range :: Int -> [Int]
range 0 = []
range i = i : range (i - 1)

handle :: Int -> Int -> Int -> String
handle first second token =
  let by1 = token `mod` first == 0
      by2 = token `mod` second == 0
  in
    case (by1, by2) of
      (True, True) -> "FB"
      (True, _) -> "F"
      (_, True) -> "B"
      (_, _) -> filter ((/=) '\"') $ show token

handleLine :: String -> String
handleLine s =
    let [first, second, max] = map read $ words s
        tokens = map (handle first second) $ reverse $ range max
    in drop 1 $ foldl (\a b -> a++" "++b) "" $ tokens

main :: IO ()
main = do
  [inpFile] <- getArgs
  input <- readFile inpFile
  let output = map handleLine $ lines input
  mapM_ putStrLn output
  
