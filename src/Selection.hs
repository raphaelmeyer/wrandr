module Selection
  ( empty,
    selected,
    onSelect,
  )
where

import qualified Data.Map.Strict as Map
import qualified Model
import qualified Monitor

empty :: Model.Selection
empty = Model.Selection Map.empty

selected :: Model.Selection -> Monitor.Name -> Maybe Monitor.Mode
selected (Model.Selection m) name = Map.lookup name m

onSelect :: Model.Selection -> Model.Focus -> Model.Selection
onSelect sel (Model.Focus Nothing _) = sel
onSelect sel (Model.Focus _ Nothing) = sel
onSelect (Model.Selection m) (Model.Focus (Just name) (Just mode)) =
  case Map.lookup name m of
    Just existing | existing == mode -> Model.Selection (Map.delete name m)
    _ -> Model.Selection (Map.insert name mode m)
