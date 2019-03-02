module Main where

import RIO
import Spec
import System.IO    (BufferMode (LineBuffering), stdout)
import Test.Hspec

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hspec spec
