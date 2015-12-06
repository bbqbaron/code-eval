import Control.Applicative
import System.Environment (getArgs)

import Debug.Trace

withSplit :: ([Float] -> Float) -> [Float] -> Float
withSplit fn l =
  let l1 = take 1000 l
      l2 = take 1000 $ drop 1000 l
  in ((fn l1) - (fn l2)) / 2000

slope :: [Float] -> Float
slope = withSplit $ ((flip (/) 1000) . sum)

ampSlope :: [Float] -> Float
ampSlope = withSplit $ (\l -> (maximum l) - (minimum l))

zeroPoint :: [Float] -> Float
zeroPoint = (flip (/) 2000) . sum

amplitude :: [Float] -> Float
amplitude l = (maximum l) - (minimum l)

pointAt :: Float -> Float -> Float -> Float
pointAt origin slope x = origin + (slope * x / 2000)

data State = State {
  x :: Float,
  lastY :: Float,
  crossings :: Float
}

freq :: [Float] -> Float
freq l =
  let zp = zeroPoint l
      a = amplitude l
      dzp = slope l
      da = ampSlope l
      freq' :: State -> Float -> State
      freq' (State {x=x, lastY=y', crossings=xs}) y =
        let currentZero = pointAt zp dzp x
            currentAmp = pointAt a da x
            threshold = currentAmp / 10
            ascending = y' <= 0
            gap = y - currentZero
            crossed = if ascending then gap > threshold else gap < (threshold * (-1))
        in State {
              x = x+1,
              lastY = if crossed then gap else y',
              crossings = xs + (if crossed then 1 else 0)
            }
  in crossings $ foldl freq' (State {x=0, lastY=0, crossings=0}) l

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
