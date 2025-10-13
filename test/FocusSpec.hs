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

spec :: Spec
spec = do
  describe "focus first monitor" $ do
    it "should return nothing if there is no monitor" $ do
      Focus.first [] `shouldBe` Nothing

    it "should focus the only monitor" $ do
      Focus.first [hdmi] `shouldBe` Just (Focus.Focus "HDMI-1" Monitor.Off)

    it "should focus the first monitor" $ do
      Focus.first [hdmi, edp] `shouldBe` Just (Focus.Focus "HDMI-1" Monitor.Off)

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
