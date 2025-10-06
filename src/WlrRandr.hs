module WlrRandr (allMonitors) where

import qualified Control.Exception as Exception
import qualified Data.Text as Text
import qualified Exception
import qualified Json
import qualified Monitor
import qualified System.Exit as Exit
import qualified System.Process as Proc

allMonitors :: IO [Monitor.Info]
allMonitors = do
  text <- queryWlr
  let json = Json.parse text
  print json
  pure []

queryWlr :: IO Text.Text
queryWlr = do
  (exitCode, output, err) <- Proc.readProcessWithExitCode "wlr-randr" ["--json"] ""
  case exitCode of
    Exit.ExitSuccess -> do
      pure $ Text.pack output
    _ -> do
      Exception.throw . Exception.WlrRandrError . Text.pack $ err
