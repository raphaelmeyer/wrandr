{-# LANGUAGE OverloadedStrings #-}

module MonitorSpec where

import qualified Data.List as List
import qualified Monitor
import Test.Hspec

spec :: Spec
spec = do
  describe "sort modes" $ do
    it "should sort modes by width" $ do
      let modes = [Monitor.Mode 200 100, Monitor.Mode 300 100, Monitor.Mode 100 100]
      List.sort modes `shouldBe` [Monitor.Mode 100 100, Monitor.Mode 200 100, Monitor.Mode 300 100]

    it "should sort modes by height" $ do
      let modes = [Monitor.Mode 100 300, Monitor.Mode 100 100, Monitor.Mode 100 200]
      List.sort modes `shouldBe` [Monitor.Mode 100 100, Monitor.Mode 100 200, Monitor.Mode 100 300]

    it "should first sort by width, then by height" $ do
      let modes =
            [ Monitor.Mode 200 300,
              Monitor.Mode 100 200,
              Monitor.Mode 200 100,
              Monitor.Mode 100 300,
              Monitor.Mode 100 100
            ]
      List.sort modes
        `shouldBe` [ Monitor.Mode 100 100,
                     Monitor.Mode 100 200,
                     Monitor.Mode 100 300,
                     Monitor.Mode 200 100,
                     Monitor.Mode 200 300
                   ]

    it "should put off at the front" $ do
      let modes = [Monitor.Mode 100 300, Monitor.Off, Monitor.Mode 100 200]
      List.sort modes `shouldBe` [Monitor.Off, Monitor.Mode 100 200, Monitor.Mode 100 300]
