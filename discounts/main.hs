import Control.Applicative
import Data.List
import System.Environment (getArgs)

evenLength :: String -> Bool
evenLength = (==) 0 . flip mod 2 . length . filter (flip elem letters)

vowels :: [Char]
vowels = "aeiouyAEIOUY"

letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

nameVowelScore :: String -> Float
nameVowelScore = (*) 1.5 . fromIntegral . length . filter (flip elem vowels) . filter (flip elem letters)

nameConsScore :: String -> Float
nameConsScore = fromIntegral . length . filter (not . flip elem vowels) . filter (flip elem letters)

compress :: String -> String
compress = filter ((/=) ' ')

factors :: Int -> [Int]
factors 0 = []
factors 1 = []
factors 2 = []
factors n = filter (\i -> mod n i == 0) [2..n]

commonFactors :: String -> String -> Bool
commonFactors s1 s2 =
  let [l1, l2] = map (length
          . filter (flip elem letters)
        ) [s1 ,s2]
      [f1, f2] = map factors [l1, l2]
      common = intersect f1 f2
      hasAny = not . null $ common
  in  hasAny

score :: String -> String -> Float
score customer item =
  let multiplier = if commonFactors customer item then 1.5 else 1
      baseScore = (if evenLength item then nameVowelScore else nameConsScore) customer
  in  baseScore * multiplier

pretty :: (Show a) => a -> String
pretty = filter ((/=) '"') . show

uniqueChoiceSets :: [String] -> [String] -> [[(String, String)]]
uniqueChoiceSets customers items =
  let
      customerShortest = length customers < length items
      shortest = if customerShortest then customers else items
      longest = if customerShortest then items else customers
      choices = choose (length shortest) longest
      reorder :: (String,String) -> (String,String)
      reorder = if customerShortest then (\ (a,b) -> (b,a)) else id
  in
    concat $
      map (\ choice ->
        map (\ perm ->
            map reorder $ zip choice perm
          )
          $ permutations shortest
        )
        choices

scoreSet :: [(String, String)] -> Float
scoreSet =
    foldl (\ acc (c,i) -> acc +
              (
              score
                c i
              )
          )
       0

highestCombo :: [String] -> [String] -> Float
highestCombo customers items =
  -- here goes nothing
  let choiceSets =
          uniqueChoiceSets customers $ items
      s =
          foldl1
          (\ s1 s2 -> if (scoreSet s1) > (scoreSet s2) then s1 else s2)
          choiceSets
  in  scoreSet s

handle :: String -> Float
handle s =
  let [customers, items] = map (splitOn ',') $ reverse $ splitOn ';' $ compress s
  in  highestCombo customers items

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

choose' :: (Show a) => [[a]] -> [a] -> Int -> [a] -> [[a]]
choose' sets chosen chooseN k
  | (length chosen) >= chooseN = chosen : sets
  | null k = sets
  | otherwise =
    let h = head k
        t = tail k
        nChosen = length chosen
        picked = choose' sets (h : chosen) chooseN t
    in
        concat [picked, choose' sets chosen chooseN t]

choose :: (Show a) => Int -> [a] -> [[a]]
choose n l
  | n >= length l = [l]
  | otherwise = choose' [] [] n l

main :: IO ()
main = do
  f <- head <$> getArgs
  ls <- lines <$> readFile f
  mapM_ putStrLn (map (pretty . handle) ls)
