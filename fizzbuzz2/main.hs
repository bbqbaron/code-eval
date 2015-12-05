import Control.Applicative
import System.Environment (getArgs)

data FizzBuzz = (Bool, Bool)

data TuplePosition = First | Second

setIn :: (a,a) -> TuplePosition -> a -> (a,a)
setIn (a,b) First c = (c, b)
setIn (a,b) Second c = (a, c)

mod3 fb = setIn fb First $ squash . (flip mod 3)
mod5 fb = setIn fb Second $ squash . (flip mod 5)

parse :: Int -> String
parse i = case map (mod i) [3,5] of
    [0,0] -> "fizzbuzz"
    [0,_] -> "fizz"
    [_,0] -> "buzz"
    otherwise -> show i

main :: IO ()
main = mapM_ putStrLn $ map parse [1..100]
