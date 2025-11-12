{-# LANGUAGE OverloadedStrings #-}

module FocusSpec where

import qualified Focus
import qualified Monitor
import Test.Hspec

hdmi :: Monitor.Info
hdmi = Monitor.Info "HDMI-1" Monitor.Off []

hdmi2 :: Monitor.Info
hdmi2 = Monitor.Info "HDMI-2" Monitor.Off []

edp :: Monitor.Info
edp = Monitor.Info "eDP-1" Monitor.Off []

modes :: [Monitor.Mode]
modes =
  [ Monitor.Mode 3840 2160,
    Monitor.Mode 2560 1440,
    Monitor.Mode 1920 1080,
    Monitor.Mode 1600 900
  ]

spec :: Spec
spec = do
  describe "focus first monitor" $ do
    it "should return nothing if there is no monitor" $ do
      Focus.first [] `shouldBe` Nothing

    it "should focus the only monitor" $ do
      Focus.first [hdmi] `shouldBe` Just (Focus.Focus "HDMI-1" Monitor.Off)

    it "should focus the first monitor" $ do
      Focus.first [hdmi, edp] `shouldBe` Just (Focus.Focus "HDMI-1" Monitor.Off)

    it "should focus off if no modes are available" $ do
      let mode = Focus.mode <$> Focus.first [hdmi]
      mode `shouldBe` Just Monitor.Off

    it "should focus the current mode" $ do
      let monitor = hdmi {Monitor.current = Monitor.Mode 1920 1080, Monitor.available = modes}
          mode = Focus.mode <$> Focus.first [monitor]
      mode `shouldBe` Just (Monitor.Mode 1920 1080)

    it "should focus off if the monitor is currently off" $ do
      let monitor = hdmi {Monitor.current = Monitor.Off, Monitor.available = modes}
          mode = Focus.mode <$> Focus.first [monitor]
      mode `shouldBe` Just Monitor.Off

    it "should focus off if the current mode is not available" $ do
      let monitor = hdmi {Monitor.current = Monitor.Mode 1024 768, Monitor.available = modes}
          mode = Focus.mode <$> Focus.first [monitor]
      mode `shouldBe` Just Monitor.Off

  describe "focus next monitor" $ do
    it "should focus the first monitor if nothing is focused" $ do
      Focus.nextMonitor [hdmi, hdmi2, edp] Nothing `shouldBe` Just (Focus.Focus "HDMI-1" Monitor.Off)

    it "should return nothing if nothing is focused but there is no monitor" $ do
      Focus.nextMonitor [] Nothing `shouldBe` Nothing

    it "should return nothing if there is no monitor" $ do
      let current = Just $ Focus.Focus "Foo" Monitor.Off
      Focus.nextMonitor [] current `shouldBe` Nothing

    it "should focus the next monitor" $ do
      let current = Just $ Focus.Focus "HDMI-1" Monitor.Off
      Focus.nextMonitor [hdmi, edp, hdmi2] current `shouldBe` Just (Focus.Focus "eDP-1" Monitor.Off)

    it "should keep the current focused if it is already the last one" $ do
      let current = Just $ Focus.Focus "eDP-1" Monitor.Off
      Focus.nextMonitor [hdmi, edp] current `shouldBe` Just (Focus.Focus "eDP-1" Monitor.Off)

    it "should focus the first monitor if current focus is invalid" $ do
      let current = Just $ Focus.Focus "Foo" Monitor.Off
      Focus.nextMonitor [hdmi, edp] current `shouldBe` Just (Focus.Focus "HDMI-1" Monitor.Off)

  describe "focus previous monitor" $ do
    it "should focus the first monitor if nothing is focused" $ do
      Focus.previousMonitor [hdmi, hdmi2, edp] Nothing `shouldBe` Just (Focus.Focus "HDMI-1" Monitor.Off)

    it "should return nothing if nothing is focused but there is no monitor" $ do
      Focus.previousMonitor [] Nothing `shouldBe` Nothing

    it "should return nothing if there is no monitor" $ do
      let current = Just $ Focus.Focus "Foo" Monitor.Off
      Focus.previousMonitor [] current `shouldBe` Nothing

    it "should focus the previous monitor" $ do
      let current = Just $ Focus.Focus "eDP-1" Monitor.Off
      Focus.previousMonitor [hdmi2, hdmi, edp] current `shouldBe` Just (Focus.Focus "HDMI-1" Monitor.Off)

    it "should keep the current focused if it is already the first one" $ do
      let current = Just $ Focus.Focus "HDMI-1" Monitor.Off
      Focus.previousMonitor [hdmi, edp] current `shouldBe` Just (Focus.Focus "HDMI-1" Monitor.Off)

    it "should focus the first monitor if current focus is invalid" $ do
      let current = Just $ Focus.Focus "Foo" Monitor.Off
      Focus.previousMonitor [hdmi, edp] current `shouldBe` Just (Focus.Focus "HDMI-1" Monitor.Off)
