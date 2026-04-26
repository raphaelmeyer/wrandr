module Model where

import qualified Data.Map.Strict as Map
import qualified Monitor

type Monitors = [Monitor.Info]

data Focus = Focus
  { focusedMonitor :: Maybe Monitor.Name,
    focusedMode :: Maybe Monitor.Mode
  }
  deriving (Eq, Show)

newtype Selection = Selection (Map.Map Monitor.Name Monitor.Mode)

data Model = Model
  { monitors :: [Monitor.Info],
    focus :: Focus,
    selection :: Selection
  }
