main = do
  mapM_ putStrLn $ map show $ filter ((/=) 0 . flip mod 2) [1..99]
