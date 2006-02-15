module Main where

import Test.HUnit
import Data.Edison.Test.Driver

main = do 
  runTestTT edisonTests
  return ()
