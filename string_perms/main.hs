import Control.Applicative
import System.Environment (getArgs)

subPerms :: String -> Char -> [String]
subPerms l i =
  let other = filter (/= i) l
  in map ((:) i) (perms other)

perms :: String -> [String]
perms [] = []
perms [x] = [[x]]
perms [x,y] = [[x,y],[y,x]]
perms l = concat $ map (subPerms l) l

join :: [String] -> String
join = drop 1 . foldl (\acc s -> acc ++ "," ++ s) ""

sorted :: [String] -> Bool
sorted [] = True
sorted [x] = True
sorted [x1,x2] = x1 < x2
sorted l = fst $ foldl (\(b, s1) s2 -> (b && (s1 < s2), s2)) (True, "") l

sort :: [String] -> [String]
sort [] = []
sort [s] = [s]
sort [s1,s2] = if s1 < s2 then [s1,s2] else [s2,s1]
sort l
  | sorted l = l
  | otherwise =
    let (s1:s2:ss) = l
    in sort $ if s1 < s2
      then s1 : (sort (s2 : ss))
      else s2 : (sort (s1 : ss))

handle :: String -> String
handle = join . sort . perms

main :: IO ()
main = do
  f <- head <$> getArgs
  ls <- lines <$> readFile f
  let results = map handle ls
  mapM_ putStrLn results
