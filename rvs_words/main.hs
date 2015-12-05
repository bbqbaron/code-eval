import Control.Applicative
import Data.List
import System.Environment (getArgs)

handle :: String -> IO ()
handle "" = do pure ()
handle s = putStrLn $ intercalate " " $ reverse $ words s

main :: IO ()
main = do
  [inpFile] <- getArgs
  input <- readFile inpFile
  mapM_ handle $ lines input
