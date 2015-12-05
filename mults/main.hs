import Control.Applicative
import System.Environment (getArgs)

log10 :: Float -> Float
log10 = logBase 10

pad :: Integer -> String
pad i =
  let len = ((floor . log10 . fromInteger) i) + 1
      padding = take (4 - len) $ repeat ' '
  in padding ++ (show i)

multLine :: Integer -> String
multLine i =
  let lines = (show i) : (map (pad . (* i)) [2..12]) :: [String]
  in concat lines

main :: IO ()
main = do
  mapM_ putStrLn $ map multLine [1..12]
