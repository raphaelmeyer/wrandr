module Application (run) where

import qualified WlrRandr

run :: IO ()
run = do
  monitors <- WlrRandr.allMonitors
  print monitors
