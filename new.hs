import System.Directory
import System.Environment (getArgs)

main = do
  cName <- head <$> getArgs
  createDirectory cName
  copyFile "tpl.hs" (cName ++ "/main.hs")
  writeFile (cName ++ "/input.txt") ""
