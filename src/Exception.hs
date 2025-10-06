{-# LANGUAGE OverloadedStrings #-}

module Exception where

import qualified Control.Exception as Exception
import qualified Data.Text as Text

data ApplicationException
  = WlrRandrError Text.Text

instance Show ApplicationException where
  show (WlrRandrError message) =
    Text.unpack $
      Text.concat ["wlr-randr returned error: '", message, "'"]

instance Exception.Exception ApplicationException
