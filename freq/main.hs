import Control.Applicative
import System.Environment (getArgs)

import Debug.Trace

withSplit :: ([Float] -> Float) -> [Float] -> Float
withSplit fn l =
  let l1 = take 1000 l
      l2 = take 1000 $ drop 1000 l
  in ((fn l2) - (fn l1))

slopeOf :: [Float] -> Float
slopeOf = withSplit ((flip (/) 1000) . sum)

zeroPointOf :: [Float] -> Float
zeroPointOf = (flip (/) 2000) . sum

pointAt :: Float -> Float -> Float -> Float
pointAt center slope x =
  let dy = (x - 1000) * slope / 1000
  in  center + dy

data State = State {
  x :: Float,
  peaks :: Float,
  inPeak :: Bool
}

tag :: (Show a) => String -> a -> a
tag s a = trace (s ++ (show a)) a

freq :: [Float] -> Int
freq l =
  let zeroPoint = tag "zp: " $ zeroPointOf l
      slope = tag "slope: " $ slopeOf l
      freq' :: State -> Float -> State
      freq' s y =
        let threshold = pointAt zeroPoint slope $ x s
            inPlay = y > threshold
            inside = inPeak s
            x' = 1 + (x s)
            gone = inside && not inPlay
            peaks' = (peaks s) + (if gone then trace ("leaving at: " ++ (show y)) 1 else 0)
            inPeak' = inPlay
        in  State {x = x', peaks = peaks', inPeak = inPeak'}
  in  truncate . ((*) 10) . peaks $ foldl freq' (State {x = 0, peaks = 0, inPeak = False}) l

pretty :: (Show a) => a -> String
pretty = filter ((/=) '"') . show

handle :: String -> String
handle = pretty . freq . (map read) . words

main :: IO ()
main = do
  f <- head <$> getArgs
  ls <- lines <$> readFile f
  let results = map handle ls
  mapM_ putStrLn results
