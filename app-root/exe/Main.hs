module Main (main) where

import qualified Data.Text.IO as TIO
import           Greeting     (sayHi)

main :: IO ()
main = do
  TIO.putStrLn $ sayHi "Haskell"
