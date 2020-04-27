module Main where

import Test.Tasty
import Tree (tests)

main :: IO ()
main = do
  tests >>= defaultMain

