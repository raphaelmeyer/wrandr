module WlrRandr (allMonitors) where

import qualified Monitor

-- wlr-randr --json
allMonitors :: IO [Monitor.Info]
allMonitors = pure []
