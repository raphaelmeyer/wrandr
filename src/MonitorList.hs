{-# LANGUAGE OverloadedStrings #-}

module MonitorList (from) where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Json
import qualified Monitor

from :: Json.Json -> [Monitor.Info]
from (Json.Array monitors) = Maybe.mapMaybe monitorInfo monitors
from _ = []

monitorInfo :: Json.Json -> Maybe Monitor.Info
monitorInfo (Json.Object monitor) = do
  name <- Map.lookup "name" monitor >>= string
  modes <- Map.lookup "modes" monitor >>= array
  current <- currentMode modes
  let available = List.nub $ Maybe.mapMaybe monitorMode modes
  pure $ Monitor.Info name current available
monitorInfo _ = Nothing

currentMode :: [Json.Json] -> Maybe Monitor.Mode
currentMode (mode : modes) =
  if isCurrent mode
    then monitorMode mode
    else currentMode modes
currentMode [] = Just Monitor.Off

isCurrent :: Json.Json -> Bool
isCurrent (Json.Object mode) =
  case Map.lookup "current" mode >>= boolean of
    Just value -> value
    _ -> False
isCurrent _ = False

monitorMode :: Json.Json -> Maybe Monitor.Mode
monitorMode (Json.Object mode) = do
  width <- truncate <$> (Map.lookup "width" mode >>= number)
  height <- truncate <$> (Map.lookup "height" mode >>= number)
  pure $ Monitor.Mode width height
monitorMode _ = Nothing

string :: Json.Json -> Maybe Text.Text
string (Json.String value) = Just value
string _ = Nothing

number :: Json.Json -> Maybe Double
number (Json.Number value) = Just value
number _ = Nothing

boolean :: Json.Json -> Maybe Bool
boolean (Json.Boolean value) = Just value
boolean _ = Nothing

array :: Json.Json -> Maybe [Json.Json]
array (Json.Array values) = Just values
array _ = Nothing
