module Focus
  ( first,
    previousMonitor,
    nextMonitor,
  )
where

import qualified Model
import qualified Monitor

first :: [Monitor.Info] -> Model.Focus
first [] = Model.Focus Nothing Nothing
first (m : _) = Model.Focus (Just $ Monitor.name m) (initialMode m)

previousMonitor :: [Monitor.Info] -> Model.Focus -> Model.Focus
previousMonitor _ focus = focus

nextMonitor :: [Monitor.Info] -> Model.Focus -> Model.Focus
nextMonitor _ focus = focus

initialMode :: Monitor.Info -> Maybe Monitor.Mode
initialMode m = case findMode (Monitor.available m) (Monitor.current m) of
  Just mode -> Just mode
  Nothing -> case Monitor.available m of
    [] -> Nothing
    (a : _) -> Just a

findMode :: [Monitor.Mode] -> Monitor.Mode -> Maybe Monitor.Mode
findMode [] _ = Nothing
findMode (a : as) current =
  if current == a
    then Just a
    else findMode as current
