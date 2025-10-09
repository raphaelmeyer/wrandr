{-# LANGUAGE OverloadedStrings #-}

module UI (attributes, draw, Name, State (..)) where

import qualified Brick.AttrMap as Attr
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as Border
import qualified Brick.Widgets.Core as Core
import qualified Data.Text as Text
import qualified Graphics.Vty as Vty
import qualified Monitor

data State = State
  { sMonitors :: [Monitor.Info]
  }

type Name = ()

appTitle :: Text.Text
appTitle = "wrandr"

attributes :: Attr.AttrMap
attributes =
  Attr.attrMap
    Vty.defAttr
    []

draw :: State -> [T.Widget Name]
draw state =
  [ Core.hBox
      [ Border.borderWithLabel
          (Core.txt appTitle)
          (monitorListWidget $ sMonitors state)
      ]
  ]

monitorListWidget :: [Monitor.Info] -> T.Widget Name
monitorListWidget monitors = Core.hBox $ map monitorWidget monitors

monitorWidget :: Monitor.Info -> T.Widget Name
monitorWidget monitor = Core.vBox [Core.txt . Monitor.name $ monitor]
