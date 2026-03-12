module Model where

import qualified Monitor

type Monitors = [Monitor.Info]

data Focus = Focus
  { focusedMonitor :: Maybe Monitor.Name,
    focusedMode :: Maybe Monitor.Mode
  }
  deriving (Eq, Show)

data Selection = Selection

data Model = Model
  { monitors :: [Monitor.Info],
    focus :: Focus,
    selection :: Selection
  }
