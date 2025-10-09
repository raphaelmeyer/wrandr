module Application (run) where

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Graphics.Vty as Vty
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
  pure $ UI.State monitors

handleEvent :: T.BrickEvent UI.Name e -> T.EventM UI.Name UI.State ()
handleEvent (T.VtyEvent e) = case e of
  Vty.EvKey Vty.KEsc [] -> M.halt
  Vty.EvKey (Vty.KChar 'q') [] -> M.halt
  _ -> pure ()
handleEvent _ = pure ()
