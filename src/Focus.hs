module Focus
  ( Focus (..),
    first,
    nextMonitor,
    previousMonitor,
  )
where

import qualified Data.Text as Text
import qualified Monitor

data Focus = Focus
  { monitor :: Text.Text,
    mode :: Monitor.Mode
  }
  deriving (Eq, Show)

first :: [Monitor.Info] -> Maybe Focus
first [] = Nothing
first (m : _) = Just $ Focus (Monitor.name m) Monitor.Off

previousMonitor :: [Monitor.Info] -> Maybe Focus -> Maybe Focus
previousMonitor monitors focus =
  maybe (first monitors) Just $
    focus
      >>= findPreviousMonitor monitors
      >>= \prev -> Just $ Focus (Monitor.name prev) Monitor.Off

nextMonitor :: [Monitor.Info] -> Maybe Focus -> Maybe Focus
nextMonitor monitors focus = case focus of
  Just f -> nextMonitor' monitors f
  Nothing -> first monitors

nextMonitor' :: [Monitor.Info] -> Focus -> Maybe Focus
nextMonitor' [] _ = Nothing
nextMonitor' ms f = case findNextMonitor ms f of
  Just next -> Just $ Focus (Monitor.name next) Monitor.Off
  Nothing -> first ms

findPreviousMonitor :: [Monitor.Info] -> Focus -> Maybe Monitor.Info
findPreviousMonitor [] _ = Nothing
findPreviousMonitor [m] f =
  if Monitor.name m == monitor f
    then Just m
    else Nothing
findPreviousMonitor (left : right : ms) f =
  if Monitor.name right == monitor f
    then Just left
    else findPreviousMonitor (right : ms) f

findNextMonitor :: [Monitor.Info] -> Focus -> Maybe Monitor.Info
findNextMonitor [] _ = Nothing
findNextMonitor [m] f =
  if Monitor.name m == monitor f
    then Just m
    else Nothing
findNextMonitor (left : right : ms) f =
  if Monitor.name left == monitor f
    then Just right
    else findNextMonitor (right : ms) f
