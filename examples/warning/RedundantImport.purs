-- @shouldWarnWith RedundantImport
module Main where

import Prelude
-- map is already part of the Prelude
import Data.Functor (map)

main :: Unit
main = unit
