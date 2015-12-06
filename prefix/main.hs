import Control.Applicative
import System.Environment (getArgs)

data Result a = Err String | OK a

instance Functor Result where
  fmap fn (OK v) = OK $ fn v
  fmap _ (Err s) = Err s

instance Applicative Result where
  pure = OK
  (<*>) (Err s) _ = Err s
  (<*>) _ (Err s) = Err s
  (<*>) (OK fn) (OK v) = OK $ fn v

instance Monad Result where
  (>>=) (Err s) _ = Err s
  (>>=) (OK v) fn = fn v

instance (Show a) => Show (Result a) where
  show (OK r) = show r
  show (Err s) = s

pretty :: (Show a) => a -> String
pretty = filter ((/=) '"') . show

data Token = Num Float | Op (Float -> Float -> Float)

instance Show Token where
  show (Num i) = show i
  show (Op (+)) = "+"
  show (Op (-)) = "-"
  show (Op (/)) = "/"
  show (Op (*)) = "*"

resolve :: [Token] -> Token -> Result [Token]
resolve [] tkn = OK [tkn]
resolve [Num i] (Num i2) = Err "Two arguments found with no operations on stack"
resolve [Num i] (Op o) = Err "Operation found after single int"
resolve [o] tkn = OK [tkn, o]
resolve [Num i, Op o] (Num i2) = OK [Num $ o i i2]
resolve [tkn2, tkn1] tkn3 = OK [tkn3, tkn2, tkn1]
resolve (tkn2:tkn:tkns) tkn3 =
  fmap (\l -> l ++ tkns) (resolve [tkn2,tkn] tkn3)

parse :: String -> Token
parse s = case s of
  "+" -> Op (+)
  "-" -> Op (-)
  "/" -> Op (/)
  "*" -> Op (*)
  otherwise -> Num $ read s

foldA :: (Monad f) => (b -> a -> (f b)) -> f b -> [a] -> f b
foldA fn = foldl (\acc new -> acc >>= (\inner -> fn inner new))

polish :: [String] -> Int
polish l =
  case foldA resolve (OK []) $ map parse l of
    OK [Num i] -> truncate i
    Err s -> 0
    OK l2 -> 0

handle :: String -> String
handle = pretty . polish . words

main :: IO ()
main = do
  f <- head <$> getArgs
  ls <- lines <$> readFile f
  let results = map handle ls
  mapM_ putStrLn results
