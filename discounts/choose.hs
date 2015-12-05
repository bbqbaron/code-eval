import Debug.Trace

tag :: (Show a) => String -> a -> a
tag l a = trace l $ traceShowId a

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
  | (length chosen) >= chooseN = tag "done" $ chosen : sets
  | null k = sets
  | otherwise =
    let h = head k
        t = tail k
        nChosen = length chosen
        picked = tag "picked" $ choose' sets (h : chosen) chooseN t
    in
        concat [picked, choose' sets chosen chooseN t]

choose :: (Show a) => Int -> [a] -> [[a]]
choose n l
  | n >= length l = [l]
  | otherwise = choose' [] [] n l

main = do
  -- putStrLn . show $ map (choose' [] [] 6) $ map (splitOn ',') $ reverse $ splitOn ';' $ "Jareau Wade,Rob Eroh,Mahmoud Abdelkader,Wenyi Cai,Justin Van Winkle,Gabriel Sinkin,Aaron Adelson;Batman No. 1,Football - Official Size,Bass Amplifying Headphones,Elephant food - 1024 lbs,Three Wolf One Moon T-shirt,Dom Perignon 2000 Vintage"
  putStrLn . show $ choose 1 [1,2,3]
