-- | Removes unused variables
module Language.PureScript.CoreImp.Optimizer.Unused
  ( removeCodeAfterReturnStatements
  , removeUnusedArg
  , removeUnusedFn
  , removeUndefinedApp
  ) where

import Prelude.Compat

import Language.PureScript.CoreImp.AST
import Language.PureScript.CoreImp.Optimizer.Common
import qualified Language.PureScript.Constants as C

removeCodeAfterReturnStatements :: AST -> AST
removeCodeAfterReturnStatements = everywhere (removeFromBlock go)
  where
  go :: [AST] -> [AST]
  go jss | not (any isReturn jss) = jss
         | otherwise = let (body, ret : _) = break isReturn jss in body ++ [ret]
  isReturn (Return _ _) = True
  isReturn (ReturnNoResult _) = True
  isReturn _ = False

removeUnusedArg :: AST -> AST
removeUnusedArg = everywhere convert
  where
  convert (Function ss name [arg] body) | arg == C.__unused = Function ss name [] body
  convert js = js

-- | The exhaustivity checker's creation of Partial constraints generates an
-- "unused" function binding. It gets passed the body of the function, wrapped
-- in an IIFE. Now that type checking is complete, we can get rid of this.
removeUnusedFn :: AST -> AST
removeUnusedFn = everywhere convert
  where
  convert (Block ss [
        VariableIntroduction _ fname _
      , Return _ (App _ (App _ (Var _ vname) _) [
          App _ (Function _ Nothing [] (Block _ originalAst)) []
        ])
    ]) | fname == C.__unused && vname == fname =
      Block ss originalAst
  convert js = js

removeUndefinedApp :: AST -> AST
removeUndefinedApp = everywhere convert
  where
  convert (App ss fn [Var _ arg]) | arg == C.undefined = App ss fn []
  convert js = js
