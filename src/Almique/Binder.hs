{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--{-# LANGUAGE OverloadedRecordFields #-}

module Almique.Binder () where

import Control.Monad.Reader
import Control.Monad.Except
import Data.Map.Strict hiding (map)

import Language.SMEIL
import qualified Almique.Analyzer as A

type BindErr = String

newtype BindM a = BindM { unBindM :: ExceptT BindErr (Reader A.AnState) a }
              deriving ( Functor, Applicative, Monad
                       , MonadError BindErr
                       , MonadReader A.AnState
                       )

runBindM :: A.AnState -> BindM Network -> Either BindErr Network
runBindM s m = let
  err = runReader (runExceptT $ unBindM m) s
  in
    case err of
      Left e -> Left e
      Right n -> Right n

-- | For each function, check that all variables in function bodies are defined
-- verify that variable kinds are correct. Furthermore, we should infer the
-- the variable kinds being used
bindFunction :: A.FunInterpState -> BindM Function
bindFunction s = undefined


-- | For each instance of Functions, check that the instantiation parameters
-- causes all dependent variables of the function being instantiated to become
-- bound variables. How to handle free variables in Externals is still unresolved.
bindInstance :: A.NewInst -> BindM Instance
bindInstance A.NewInst { A.instBindings = bindings
                       , A.inst = i@Instance
                         { params = ps } } = do
  matchBindings $ zip bindings ps
  return Instance {}
  where
    matchBindings :: [(A.Binding, Expr)] -> BindM ()
    matchBindings = undefined


bindNetwork :: BindM Network
bindNetwork = do
  busList <- elems <$> asks A.busses
  funs <- asks A.funStates
  funs' <- mapM bindFunction funs
  insts <- elems <$> asks A.instances
  insts' <- mapM bindInstance insts
  return Network { functions = funs'
                 , busses = busList
                 , instances = insts'
                 }

bindPyMod :: A.AnState -> Either BindErr Network
bindPyMod s = runBindM s bindNetwork
