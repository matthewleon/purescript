module Main where

import Control.Monad.Eff.Console (log)

-- https://github.com/purescript/purescript/issues/2941

class Test t where
  test :: forall a. t -> a

instance testUnit :: Test Unit where
  test :: forall a. Unit -> a
  test = go
    where
      go :: Unit -> a
      go = unsafeCoerce

main = log "Done"
