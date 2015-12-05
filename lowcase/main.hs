module Main where

import Control.Applicative
import Data.List
import System.Environment (getArgs)

upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
lower = "abcdefghijklmnopqrstuvwxyz"

toLower :: Char -> Char
toLower c =
  case elemIndex c upper of
    Just i -> head $ drop i lower
    otherwise -> c

handle :: String -> String
handle = map toLower

main :: IO ()
main = do
  f <- head <$> getArgs
  ls <- lines <$> readFile f
  let results = map handle ls
  mapM_ putStrLn results
