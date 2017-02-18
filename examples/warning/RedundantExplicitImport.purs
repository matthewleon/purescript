-- @shouldWarnWith RedundantExplicitImport
module Main where

import Prelude
-- Functor is already part of the Prelude
import Data.Functor (class Functor, mapFlipped)

flippyMap :: forall a b f. Functor f => f a -> (a -> b) -> f b
flippyMap = mapFlipped

main :: Unit
main = unit
