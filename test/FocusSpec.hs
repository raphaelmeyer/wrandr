{-# LANGUAGE OverloadedStrings #-}

module FocusSpec where

import qualified Focus
import qualified Model
import Monitor (Info (available))
import qualified Monitor
import Test.Hspec

hdmi :: Monitor.Info
hdmi = Monitor.Info "HDMI-1" Monitor.Off []

hdmi2 :: Monitor.Info
hdmi2 = Monitor.Info "HDMI-2" Monitor.Off []

edp :: Monitor.Info
edp = Monitor.Info "eDP-1" Monitor.Off []

uhd4k :: Monitor.Mode
uhd4k = Monitor.Mode 3840 2160

wqhd :: Monitor.Mode
wqhd = Monitor.Mode 2560 1440

fhd :: Monitor.Mode
fhd = Monitor.Mode 1920 1080

hdplus :: Monitor.Mode
hdplus = Monitor.Mode 1600 900

modes :: [Monitor.Mode]
modes =
  [ uhd4k,
    wqhd,
    fhd,
    hdplus
  ]

spec :: Spec
spec = do
  describe "focus first monitor" $ do
    it "should focus nothing if there is no monitor" $ do
      Focus.first [] `shouldBe` Model.Focus Nothing Nothing

    it "should focus the monitor if there is only one" $ do
      Focus.first [hdmi] `shouldBe` Model.Focus (Just "HDMI-1") Nothing

    it "should focus the first monitor in the list" $ do
      (Model.focusedMonitor . Focus.first $ [edp, hdmi2, hdmi]) `shouldBe` Just "eDP-1"

    it "should not focus a mode when non are available" $ do
      (Model.focusedMode . Focus.first $ [edp, hdmi2]) `shouldBe` Nothing

    it "should focus the current mode" $ do
      let monitors = [hdmi {Monitor.current = fhd, Monitor.available = [fhd, wqhd]}]
      (Model.focusedMode . Focus.first $ monitors) `shouldBe` Just fhd

    it "should focus the first available mode if the current mode is not available" $ do
      let monitors = [hdmi {Monitor.current = fhd, Monitor.available = [wqhd, hdplus]}]
      (Model.focusedMode . Focus.first $ monitors) `shouldBe` Just wqhd

  describe "nextMonitor" $ do
    it "should focus nothing when there are no monitors" $ do
      let f = Model.Focus (Just "HDMI-1") (Just fhd)
      Focus.nextMonitor [] f `shouldBe` Model.Focus Nothing Nothing

    it "should keep focus unchanged when there is only one monitor" $ do
      let monitors = [hdmi {Monitor.available = modes}]
      let f = Model.Focus (Just "HDMI-1") (Just fhd)
      Focus.nextMonitor monitors f `shouldBe` f

    it "should keep focus unchanged when already on the last monitor" $ do
      let monitors = [hdmi {Monitor.available = modes}, hdmi2 {Monitor.available = modes}]
      let f = Model.Focus (Just "HDMI-2") (Just fhd)
      Focus.nextMonitor monitors f `shouldBe` f

    it "should focus the first monitor when no monitor is focused" $ do
      let monitors = [hdmi {Monitor.current = fhd, Monitor.available = [fhd, wqhd]}, hdmi2 {Monitor.available = modes}]
      let f = Model.Focus Nothing Nothing
      Focus.nextMonitor monitors f `shouldBe` Model.Focus (Just "HDMI-1") (Just fhd)

    it "should move to the next monitor" $ do
      let monitors = [hdmi {Monitor.available = modes}, hdmi2 {Monitor.available = modes}]
      let f = Model.Focus (Just "HDMI-1") (Just fhd)
      Model.focusedMonitor (Focus.nextMonitor monitors f) `shouldBe` Just "HDMI-2"

    it "should focus the current mode of the next monitor if it is available" $ do
      let monitors = [hdmi {Monitor.current = fhd, Monitor.available = [fhd]}, hdmi2 {Monitor.current = wqhd, Monitor.available = [uhd4k, wqhd]}]
      let f = Model.Focus (Just "HDMI-1") (Just fhd)
      Model.focusedMode (Focus.nextMonitor monitors f) `shouldBe` Just wqhd

    it "should focus the first available mode of the next monitor if its current mode is not available" $ do
      let monitors = [hdmi {Monitor.current = fhd, Monitor.available = [fhd]}, hdmi2 {Monitor.current = uhd4k, Monitor.available = [wqhd, fhd]}]
      let f = Model.Focus (Just "HDMI-1") (Just fhd)
      Model.focusedMode (Focus.nextMonitor monitors f) `shouldBe` Just wqhd

    it "should focus the first monitor when the focused monitor is not found in the list" $ do
      let monitors = [hdmi {Monitor.current = fhd, Monitor.available = [fhd]}, hdmi2 {Monitor.available = modes}]
      let f = Model.Focus (Just "eDP-1") (Just fhd)
      Model.focusedMonitor (Focus.nextMonitor monitors f) `shouldBe` Just "HDMI-1"
