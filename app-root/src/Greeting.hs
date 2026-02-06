module Greeting (sayHi) where

import           Data.Text (Text)

sayHi :: Text -> Text
sayHi = ("Hi, " <>)
