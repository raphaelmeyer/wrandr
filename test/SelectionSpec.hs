{-# LANGUAGE OverloadedStrings #-}

module SelectionSpec where

import qualified Model
import qualified Monitor
import qualified Selection
import Test.Hspec

fhd :: Monitor.Mode
fhd = Monitor.Mode 1920 1080

wqhd :: Monitor.Mode
wqhd = Monitor.Mode 2560 1440

hdmi :: Monitor.Name
hdmi = "HDMI-1"

hdmi2 :: Monitor.Name
hdmi2 = "HDMI-2"

spec :: Spec
spec = do
  describe "empty" $ do
    it "has no selection for any monitor" $ do
      Selection.selected Selection.empty hdmi `shouldBe` Nothing

  describe "onSelect" $ do
    it "selects focused mode for focused monitor" $ do
      let focusHdmiFhd = Model.Focus (Just hdmi) (Just fhd)
      let withFhd = Selection.onSelect Selection.empty focusHdmiFhd
      Selection.selected withFhd hdmi `shouldBe` Just fhd

    it "does nothing when no monitor is focused" $ do
      let noMonitor = Model.Focus Nothing (Just fhd)
      let result = Selection.onSelect Selection.empty noMonitor
      Selection.selected result hdmi `shouldBe` Nothing

    it "does nothing when no mode is focused" $ do
      let noMode = Model.Focus (Just hdmi) Nothing
      let result = Selection.onSelect Selection.empty noMode
      Selection.selected result hdmi `shouldBe` Nothing

    it "removes selection when focused mode is already selected" $ do
      let focusHdmiFhd = Model.Focus (Just hdmi) (Just fhd)
      let withFhd = Selection.onSelect Selection.empty focusHdmiFhd
      let deselected = Selection.onSelect withFhd focusHdmiFhd
      Selection.selected deselected hdmi `shouldBe` Nothing

    it "replaces selection when a different mode is focused" $ do
      let focusFhd = Model.Focus (Just hdmi) (Just fhd)
      let focusWqhd = Model.Focus (Just hdmi) (Just wqhd)
      let withFhd = Selection.onSelect Selection.empty focusFhd
      let withWqhd = Selection.onSelect withFhd focusWqhd
      Selection.selected withWqhd hdmi `shouldBe` Just wqhd

    it "can select modes for multiple monitors independently" $ do
      let focusHdmi = Model.Focus (Just hdmi) (Just fhd)
      let focusHdmi2 = Model.Focus (Just hdmi2) (Just wqhd)
      let withBoth = Selection.onSelect (Selection.onSelect Selection.empty focusHdmi) focusHdmi2
      Selection.selected withBoth hdmi `shouldBe` Just fhd
      Selection.selected withBoth hdmi2 `shouldBe` Just wqhd
