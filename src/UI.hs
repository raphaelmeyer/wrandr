{-# LANGUAGE OverloadedStrings #-}

module UI (attributes, draw, Name, State (..)) where

import qualified Brick.AttrMap as Attr
import qualified Brick.Types as T
import qualified Brick.Util as Util
import qualified Brick.Widgets.Border as Border
import qualified Brick.Widgets.Center as Center
import qualified Brick.Widgets.Core as Core
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Focus
import qualified Graphics.Vty as Vty
import qualified Monitor

data State = State
  { sMonitors :: [Monitor.Info],
    sFocus :: Maybe Focus.Focus
  }

data Name = Name Text.Text deriving (Eq, Ord, Show)

appTitle :: Text.Text
appTitle = "wrandr"

aFocus :: Attr.AttrName
aFocus = Attr.attrName "focus"

attributes :: Attr.AttrMap
attributes =
  Attr.attrMap
    Vty.defAttr
    [(aFocus, Util.fg Vty.magenta)]

draw :: State -> [T.Widget Name]
draw state = [applicationWidget state]

applicationWidget :: State -> T.Widget Name
applicationWidget state =
  titleFrameWidget appTitle $
    Core.vBox
      [ monitorListWidget state,
        Border.hBorder,
        legendWidget
      ]

monitorListWidget :: State -> T.Widget Name
monitorListWidget state =
  Core.padAll 1
    . Core.hBox
    $ map (Core.padLeft (Core.Pad 1) . monitorWidget (sFocus state))
    $ sMonitors state

monitorWidget :: Maybe Focus.Focus -> Monitor.Info -> T.Widget Name
monitorWidget focus monitor =
  monitorFrameWidget (Maybe.isJust focusedMode) monitor
    . Core.hLimit 20
    . Core.vBox
    $ [ currentModeWidget monitor,
        Border.hBorder,
        availableModesWidget focusedMode monitor
      ]
  where
    focusedMode = focus >>= getFocusedMode monitor

monitorFrameWidget :: Bool -> Monitor.Info -> T.Widget Name -> T.Widget Name
monitorFrameWidget focused monitor =
  Border.borderWithLabel
    . Core.padLeftRight 1
    $ title monitor
  where
    title = style . Core.txt . Monitor.name
    style = if focused then Core.withAttr aFocus else id

currentModeWidget :: Monitor.Info -> T.Widget Name
currentModeWidget =
  Core.padTop (Core.Pad 1)
    . Center.hCenter
    . Core.str
    . show
    . Monitor.current

availableModesWidget :: Maybe Monitor.Mode -> Monitor.Info -> T.Widget Name
availableModesWidget focusedMode monitor =
  let name = Monitor.name monitor
   in Core.vLimit 4
        . Core.viewport (Name name) T.Vertical
        . Core.vBox
        $ map (availableModeWidget focusedMode) (Monitor.available monitor)

availableModeWidget :: Maybe Monitor.Mode -> Monitor.Mode -> T.Widget Name
availableModeWidget focusedMode mode =
  Core.padLeft Core.Max
    . Core.padRight (Core.Pad 1)
    . style
    . Core.str
    . show
    $ mode
  where
    focused = isModeFocused focusedMode mode
    style = if focused then Core.visible . Core.withAttr aFocus else id

legendWidget :: T.Widget Name
legendWidget = Core.hBox $ map columnWidget legendItems
  where
    columnWidget = Core.vBox . map (Core.padLeftRight 2 . Core.txt)

legendItems :: [[Text.Text]]
legendItems =
  [ [ "↑ ↓ Focus mode",
      "← → Focus monitor"
    ],
    [ "ENTER Select mode",
      "a     Apply"
    ],
    [ "q ESC Quit"
    ]
  ]

titleFrameWidget :: Text.Text -> T.Widget Name -> T.Widget Name
titleFrameWidget = Border.borderWithLabel . Core.padLeftRight 1 . Core.txt

getFocusedMode :: Monitor.Info -> Focus.Focus -> Maybe Monitor.Mode
getFocusedMode m focus =
  if Monitor.name m == Focus.monitor focus
    then Just $ Focus.mode focus
    else Nothing

isModeFocused :: Maybe Monitor.Mode -> Monitor.Mode -> Bool
isModeFocused Nothing _ = False
isModeFocused (Just focused) mode = mode == focused
