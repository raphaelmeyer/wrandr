module Focus
  ( first,
    previousMonitor,
    nextMonitor,
    nextMode,
  )
where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Model
import qualified Monitor

first :: [Monitor.Info] -> Model.Focus
first [] = Model.Focus Nothing Nothing
first (m : _) = Model.Focus (Just $ Monitor.name m) (initialMode m)

previousMonitor :: [Monitor.Info] -> Model.Focus -> Model.Focus
previousMonitor [] _ = Model.Focus Nothing Nothing
previousMonitor monitors (Model.Focus Nothing _) = first monitors
previousMonitor monitors focus =
  case Model.focusedMonitor focus of
    Nothing -> focus
    Just name ->
      case break (\m -> Monitor.name m == name) monitors of
        ([], _) -> focus
        (_, []) -> first monitors
        (before, _) ->
          let previous = last before
           in Model.Focus (Just $ Monitor.name previous) (initialMode previous)

nextMonitor :: [Monitor.Info] -> Model.Focus -> Model.Focus
nextMonitor [] _ = Model.Focus Nothing Nothing
nextMonitor monitors (Model.Focus Nothing _) = first monitors
nextMonitor monitors focus =
  case Model.focusedMonitor focus of
    Nothing -> focus
    Just name ->
      case dropWhile (\m -> Monitor.name m /= name) monitors of
        (_ : next : _) -> Model.Focus (Just $ Monitor.name next) (initialMode next)
        [_] -> focus
        _ -> first monitors

nextMode :: [Monitor.Info] -> Model.Focus -> Model.Focus
nextMode [] _ = Model.Focus Nothing Nothing
nextMode monitors (Model.Focus Nothing _) = first monitors
nextMode monitors focus = Maybe.fromMaybe focus $ do
  name <- Model.focusedMonitor focus
  monitor <- List.find (\m -> Monitor.name m == name) monitors
  mode <- Model.focusedMode focus
  (_ : next : _) <- Just $ dropWhile (/= mode) (Monitor.available monitor)
  return $ Model.Focus (Just name) (Just next)

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
