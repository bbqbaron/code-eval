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
  inPeak :: Float,
  startedInPeak :: Maybe Bool,
  beganCompletePeak :: Bool,
  partialInPlays :: Float
}

tag :: (Show a) => String -> a -> a
tag s a = trace (s ++ (show a)) a

freq :: [Float] -> Int
freq l =
  let zeroPoint = tag "zp: " $ zeroPointOf l
      slope = tag "slope: " $ slopeOf l
      freq' :: State -> Float -> State
      freq' state y =
        let threshold = pointAt zeroPoint slope $ x state
            inPlay = y > threshold
            inside = (inPeak state) > 0
            x' = 1 + (x state)
            gone = inside && not inPlay
            peaks' = (peaks state) + (if gone then trace ("leaving at: " ++ (show y)) 1 else 0)
            inPeak' = if gone then 0 else (inPeak state) + (if inPlay then 1 else 0)
            beganCompletePeak' = if beganCompletePeak state then True else inPlay && (not inside)
            startedInPeak' = case startedInPeak state of
              Nothing -> Just inPlay
              Just b -> Just b
            partialInPlays' = if beganCompletePeak' then partialInPlays state else inPeak'
        in  State {x = x', peaks = peaks', inPeak = inPeak', startedInPeak = startedInPeak', beganCompletePeak = beganCompletePeak', partialInPlays = partialInPlays'}
      final = foldl freq' (State {x = 0, peaks = 0, inPeak = 0, startedInPeak = Nothing, beganCompletePeak = False, partialInPlays = 0}) l
      partials = tag "partials: " $ (tag "final: " $ inPeak final) + (tag "start: " $ partialInPlays final)
      waveLength = tag "wavelength: " $ 2000 / (peaks final)
      roundUp = tag "round up: " $ partials > waveLength / 2
      peaks' = peaks final + if roundUp then 1 else 0
  in  truncate $ (*) 10 $ peaks'

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
