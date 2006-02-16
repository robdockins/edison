-- Copyright (c) 2006 Robert Dockins
-- See COPYRIGHT file for terms and conditions.

module Main where

import Test.HUnit
import Data.Edison.Test.Driver

main = do 
  runTestTT edisonTests
  return ()
