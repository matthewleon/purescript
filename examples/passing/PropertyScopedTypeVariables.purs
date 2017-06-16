module Main where

import Control.Monad.Eff.Console (log)

-- https://github.com/purescript/purescript/issues/2941

rec :: { attr :: forall a. a -> Int, mattr :: forall a. a -> Int }
rec  = { attr:  let go :: a -> Int
                    go = \_ -> 0 
                in  go

       , mattr: let go :: a -> Int
                    go = \_ -> 0
                in  go
       }

main = log "Done"
