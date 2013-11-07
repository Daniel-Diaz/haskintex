
module Main (main) where

import Haskintex
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= haskintex
