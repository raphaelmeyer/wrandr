module Application (run) where

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Data.Text as Text
import qualified Focus
import qualified Graphics.Vty as Vty
import qualified Model
import qualified Monitor
import qualified Selection
import qualified UI
import qualified WlrRandr

run :: IO ()
run = do
  initialState <- resetMonitorInfo
  _ <- M.defaultMain mkApp initialState
  pure ()

mkApp :: M.App UI.State e UI.Name
mkApp =
  M.App
    { M.appDraw = UI.draw,
      M.appChooseCursor = M.neverShowCursor,
      M.appHandleEvent = handleEvent,
      M.appStartEvent = pure (),
      M.appAttrMap = const UI.attributes
    }

resetMonitorInfo :: IO UI.State
resetMonitorInfo = do
  monitors <- WlrRandr.allMonitors
  pure $ UI.State (Model.Model monitors (Focus.first monitors) Selection.empty)

focusPreviousMonitor :: UI.State -> UI.State
focusPreviousMonitor (UI.State model) =
  UI.State model {Model.focus = Focus.previousMonitor (Model.monitors model) (Model.focus model)}

focusNextMonitor :: UI.State -> UI.State
focusNextMonitor (UI.State model) =
  UI.State model {Model.focus = Focus.nextMonitor (Model.monitors model) (Model.focus model)}

focusNextMode :: UI.State -> UI.State
focusNextMode (UI.State model) =
  UI.State model {Model.focus = Focus.nextMode (Model.monitors model) (Model.focus model)}

focusPreviousMode :: UI.State -> UI.State
focusPreviousMode (UI.State model) =
  UI.State model {Model.focus = Focus.previousMode (Model.monitors model) (Model.focus model)}

handleEvent :: T.BrickEvent UI.Name e -> T.EventM UI.Name UI.State ()
handleEvent (T.VtyEvent e) = case e of
  Vty.EvKey Vty.KLeft [] -> T.modify focusPreviousMonitor
  Vty.EvKey Vty.KRight [] -> T.modify focusNextMonitor
  Vty.EvKey Vty.KDown [] -> T.modify focusNextMode
  Vty.EvKey Vty.KUp [] -> T.modify focusPreviousMode
  Vty.EvKey Vty.KEsc [] -> M.halt
  Vty.EvKey (Vty.KChar 'q') [] -> M.halt
  Vty.EvKey (Vty.KChar 'd') [] -> T.put fakeMonitors
  _ -> pure ()
handleEvent _ = pure ()

fakeMonitors :: UI.State
fakeMonitors = UI.State (Model.Model monitors focus Selection.empty)
  where
    monitors =
      [ Monitor.Info
          (Text.pack "Foo")
          (Monitor.Mode 320 240)
          [ Monitor.Mode 1024 768,
            Monitor.Mode 640 480,
            Monitor.Mode 320 240
          ],
        Monitor.Info
          (Text.pack "Bar")
          (Monitor.Mode 1920 1200)
          [ Monitor.Mode 3840 2160,
            Monitor.Mode 2560 1440,
            Monitor.Mode 1920 1200,
            Monitor.Mode 1920 1080,
            Monitor.Mode 1600 1200,
            Monitor.Mode 1600 900
          ],
        Monitor.Info
          (Text.pack "HDMI")
          (Monitor.Mode 1920 1080)
          [ Monitor.Mode 1920 1080,
            Monitor.Mode 1280 720
          ]
      ]
    focus = Focus.first monitors
