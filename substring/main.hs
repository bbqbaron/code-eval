{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Data.Maybe (fromMaybe)
import Data.Text (pack, split, Text, toLower, unpack)
import System.Environment (getArgs)

dropLast :: [a] -> [a]
dropLast [] = []
dropLast [_] = []
dropLast l = reverse $ drop 1 $ reverse l

-- an exercise to learn monads
isBackslash :: String -> Bool
isBackslash [] = False
isBackslash [c] = c == '\\'
isBackslash l = (==) '\\' $ head $ reverse l

data Asterisk = Escaped | Unescaped | None

asterisk :: String -> Char -> Asterisk
asterisk s c =
    if c == '*'
        then if isBackslash s
            then Escaped
            else Unescaped
        else None

data Pattern = Simple String | Complex String String

parse :: String -> Pattern
parse = parse' ""
    where parse' :: String -> String -> Pattern
          parse' s "" = Simple s
          parse' s [c] =
              case asterisk s c of
                  Escaped -> Simple $ (dropLast s) ++ [c]
                  Unescaped -> Simple s
                  None -> Simple $ s ++ [c]
          parse' s (c:cs) =
              case asterisk s c of
                  Escaped -> parse' ((dropLast s) ++ [c]) cs
                  Unescaped -> Complex s cs
                  None -> parse' (s ++ [c]) cs

-- indexed recursion; is that already a thing?
find' :: String -> String -> Int -> Int
find' "" "" i = i
find' "" _ _ = -1
find' _ "" i = i
find' [c] [csub] i = if c == csub then i else -1
find' [c] (_:_) _ = -1
find' (h:t) [csub] i = if h == csub then i else -1
find' (h:t) (hsub:tsub) i =
    find' t (
        if h == hsub
            then tsub
            else (hsub:tsub)
    ) (i+1)

find :: [String] -> Bool
find [_, "*"] = True
find [_, ""] = True
find ["", sub] = False
find [string, sub] =
    case parse sub of
        Simple s -> (find' string s 0) > -1
        Complex s1 s2 ->
            let index1 = find' string s1 0
                index2 = if index1 > -1
                            then find' string s2 0
                            else -1
            in index2 >= (index1 + (length s1))
find s = False

handle :: String -> String
handle = (\b -> if b then "true" else "false") . find . (map unpack) . (split ((==) ',')) . pack

main :: IO ()
main = do
  f <- head <$> getArgs
  ls <- lines <$> readFile f
  let results = map handle ls
  mapM_ putStrLn results
