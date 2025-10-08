{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module MonitorListSpec where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.String.QQ
import qualified Json
import qualified Monitor
import qualified MonitorList
import Test.Hspec

examples :: Json.Json
examples =
  Maybe.fromJust . Json.parse $
    [s|
      [{
        "name": "eDP-1",
        "modes": [
          { "width": 1920, "height": 1200, "current": true }
        ]
      },{
        "name": "HDMI-A-1",
        "modes": [
          { "width": 3840, "height": 2160, "current": true },
          { "width": 2560, "height": 1440, "current": false },
          { "width": 1920, "height": 1080, "current": false }
        ]
      }]
    |]

modeExamples :: Json.Json
modeExamples =
  Maybe.fromJust . Json.parse $
    [s|
      [{
        "name": "HDMI-2",
        "modes": [
          { "width": 3840, "height": 2160, "current": false },
          { "width": 2560, "height": 1440, "current": false },
          { "width": 2560, "height": 1440, "current": true },
          { "width": 1920, "height": 1080, "current": true },
          { "width": 1920, "height": 1080, "current": false }
        ]
      }]
    |]

noCurrentMode :: Json.Json
noCurrentMode =
  Maybe.fromJust . Json.parse $
    [s|
      [{
        "name": "HDMI-2",
        "modes": [
          { "width": 3840, "height": 2160, "current": false },
          { "width": 2560, "height": 1440, "current": false },
          { "width": 1920, "height": 1080, "current": false }
        ]
      }]
    |]

spec :: Spec
spec = do
  describe "no monitor" $ do
    it "should return an empty list" $ do
      MonitorList.from (Json.Array []) `shouldBe` []

  describe "properties" $ do
    it "should retrieve name" $ do
      let monitors = MonitorList.from examples
      map Monitor.name monitors `shouldMatchList` ["eDP-1", "HDMI-A-1"]

    it "should retrieve current mode" $ do
      let monitors = MonitorList.from examples
      map Monitor.current monitors `shouldMatchList` [Monitor.Mode 1920 1200, Monitor.Mode 3840 2160]

    it "should retrieve available modes" $ do
      let monitors = MonitorList.from examples
      map (List.sort . Monitor.available) monitors `shouldMatchList` [[Monitor.Mode 1920 1200], [Monitor.Mode 1920 1080, Monitor.Mode 2560 1440, Monitor.Mode 3840 2160]]

  describe "modes" $ do
    it "should report each mode only once" $ do
      let modes = Monitor.available . head $ MonitorList.from modeExamples
      modes `shouldMatchList` [Monitor.Mode 1920 1080, Monitor.Mode 2560 1440, Monitor.Mode 3840 2160]

    it "should select one current mode" $ do
      let current = Monitor.current . head $ MonitorList.from modeExamples
      [Monitor.Mode 2560 1440, Monitor.Mode 1920 1080] `shouldContain` [current]
      current `shouldNotBe` Monitor.Mode 3840 2160

    it "should consider monitor off if no current mode is found" $ do
      let current = Monitor.current . head $ MonitorList.from noCurrentMode
      current `shouldBe` Monitor.Off
