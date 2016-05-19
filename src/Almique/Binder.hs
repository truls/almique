{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Almique.Binder
       ( bindPyMod
       , BindErr
       ) where

import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.Map.Strict as Map hiding (map)
import Data.Maybe (isNothing, fromMaybe)
import Data.List
import Data.Monoid

import Language.SMEIL
import qualified Almique.Analyzer as A

import Debug.Trace

type InstPorts = (Ident, ([Ident], [Ident]))
type BindErr = String

newtype BindM a = BindM { unBindM :: ExceptT BindErr (Reader A.AnState) a }
              deriving ( Functor, Applicative, Monad
                       , MonadError BindErr
                       , MonadReader A.AnState
                       )

runBindM :: A.AnState -> BindM Network -> Either BindErr Network
runBindM s m = runReader (runExceptT $ unBindM m) s

-- | Query state of named function
-- TODO: Maybe restate this in terms of withCurBlock
queryNamedIS :: String -> (A.FunInterpState -> a) -> BindM a
queryNamedIS i f = do
  sts <- asks A.funStates
  case Map.lookup i sts of
    Just v -> return $ f v
    Nothing -> throwError $ "Undefined function: " ++ i

queryFunIS :: String -> (Function -> a) -> BindM a
queryFunIS s f = f <$> queryNamedIS s A.blockFunction

-- TODO: This is basically the same function as in Analyzer: Find some way of generalizing
queryIS :: (A.FunInterpState -> a) -> (a -> b) -> BindM b
queryIS f t = do
  hasCurBlock <- isNothing <$> asks A.currentBlock
  when hasCurBlock $ throwError "Internal compiler error: Tried querying and empty block"
  (Just curBlock) <- asks A.currentBlock
  return $ t $ f curBlock

queryFunCurIS :: (Function -> a) -> BindM a
queryFunCurIS = queryIS A.blockFunction

-- |Try to see if a free variable can be resolved. Very primitive at this point,
-- only checks list of variables (At this point, we know that parameters of
-- Complete functions becomes bound at runtime)
resolveFree :: Variable -> BindM Bool
resolveFree (NamedVar n) = queryIS A.params $ elem n
resolveFree _ = throwError "resolveFree: Only NamedVar variables can be free?"

-- |Lookup variable binding. If bound, return corresponding expression. If free,
-- try resolving the binding as a parameter and return Nothing if unsuccessful
lookupBinding :: Variable -> BindM (Maybe Expr)
lookupBinding v = do
  binding <- queryIS A.bindings (Map.lookup v)
  case binding of
    Just (A.Bound e) -> return $ Just e
    Just (A.Free e@(Var var)) -> do
      resolved <- resolveFree var
      if resolved then
        return $ Just e
        else
        return Nothing
    -- TODO: Check that other expressions can be fully evaluated at this point
    Just (A.Free e) -> return Nothing
    _ -> return Nothing

withCurBlock :: A.FunInterpState -> BindM a -> BindM a
withCurBlock s = local (\c -> c { A.currentBlock = Just s })

queryBusDef :: Ident -> (Bus -> a) -> BindM a
queryBusDef i f = do
  -- FIXME: Mostly a duplicate of queryNamedIS. Consolidate!
  el <- asks A.busses
  case Map.lookup i el of
    Just v -> return $ f v
    Nothing -> throwError $ "Reference to undefined bus" ++ i

-- FIXME: Querying a bus just to see if queryBusDef will throw an
-- error... Is this bad?
checkBus :: Ident -> BindM Ident
checkBus b = queryBusDef b id >> return b

-- | For each function, check that all variables in function bodies are defined
-- verify that variable kinds are correct. Furthermore, we should infer the
-- the variable kinds being used
bindFunction :: [InstPorts] -> BindM Function
bindFunction ps = trace (show ps) $ do
  -- FIXME: Most of these mappings (and by extension the entire FunInterpState
  -- type) ended up being rather pointless.
  funName' <- queryFunCurIS funName
  funType' <- queryFunCurIS funType
  let (inPortTypes, outPortTypes) = getPortMap funName'
  inPorts <- queryIS A.mappedInBusses (`zip` inPortTypes) -- >>= mapM checkBus
  outPorts <- queryIS A.mappedOutBusses (`zip` outPortTypes) -- >>= mapM checkBus
  funParams' <- map mapFunParam <$> queryIS A.params id -- >>= genFunParam
  (funBody', funVars) <- queryFunCurIS funBody >>= mapStmts checkStmt
  funBody'' <- queryFunCurIS funBody
  -- FIXME: Using toList feels like a bad idea
  locals' <- queryIS A.bindings Map.toList >>= mapM (mapBinding funVars)
  return Function { funName = funName'
                  , funInports = inPorts
                  , funOutports = outPorts
                  , funParams = funParams'
                  , locals = locals'
                  , funBody = funBody''
                  , funType = funType'
                  }
    where
      mapFunParam :: Ident -> Decl
      -- FIXME: Kind of useless right now. The Decl in order to enable future
      -- support for default values
      mapFunParam a = (Decl $ NamedVar a) Nothing

      getPortMap :: Ident -> ([Ident], [Ident])
      getPortMap i = fromMaybe ([], []) $ lookup i ps

      -- | Map a function over Stmts and accumulate the transformed statements and
      -- the the list of variables being assigned to (=modified) by the
      -- statements. We use this to infer variable constness.
      mapStmts :: (Stmt -> BindM (Stmt, Maybe Variable))
               -> Stmts
               -> BindM (Stmts, [Variable])
      mapStmts f (Stmts (Cond cs e:rest)) = do
        a <- mapStmts f $ foldr (mappend.snd) mempty cs
        b <- mapStmts f e
        c <- mapStmts f $ Stmts rest
        return $ a <> b <> c
      mapStmts f (Stmts s) =
        mapM f s >>=
        foldM (\(stmts, vars) (stmt, var) ->
                 return ( stmts <> Stmts [stmt]
                        , vars <> fromMaybe [] ((: []) <$> var))
              )
        (mempty, mempty)

      mapBinding :: [Variable] -> (Variable, A.Binding) -> BindM Decl
      mapBinding vs (i, _b) = do
        binding <- lookupBinding i
        case binding of
          m@(Just _) -> return $ case i of
                                   BusVar _ _ -> Decl i m
                                   ConstVar _ -> Decl i m
                                   NamedVar n -> if i `notElem` vs then
                                                     Decl (ConstVar n) m
                                                   else
                                                     Decl i m
          m@Nothing -> queryFunCurIS funType >>= \case
            Skeleton -> return $ Decl i m
            _ -> throwError $ "Reference to free variable " ++ show i

-- Static checking of SMEIL
-- TODO: (See TODO note below). Maybe output a list of variables modified by the
  -- statement. Then, after all statements has been processed, match list of
  -- bindings with list of touched variables and change variables that were
  -- never touched to constants.
checkStmt :: Stmt -> BindM (Stmt, Maybe Variable)
checkStmt s@(Assign v e) = return (s, Just v)
checkStmt s = return (s, Nothing)


{- TODO:
- Check variable bindings. If variable is resolved to a futrhnction
  parameter, replace occurrences of the variable with the name of the
  parameter. We do this, because parameters will be mapped to VHDL generics.
- Infer variable constness. Variables that are never assigned to and have a default
  value are inferred as constants. To do this, keep the kind of each variable
  somewhere, maybe defaulting to constant and then change to regular variables
  if assigned to or updated.
-}
checkVar :: Variable -> BindM Variable
checkVar (ConstVar _) = throwError "Const vars are currently unsupported"
checkVar (NamedVar _) = undefined
checkVar (BusVar _ _) = undefined

-- | For each instance of Functions, check that the instantiation parameters
-- causes all dependent variables of the function being instantiated to become
-- bound variables. How to handle free variables in Externals is still unresolved.
bindInstance :: A.NewInst -> BindM Instance
bindInstance A.NewInst { A.instBindings = bindings
                       , A.inst = Instance
                         { instParams = _ps
                         , instName = name
                         , instFun = fun
                         , inBusses = inb
                         , outBusses = outb
                         }
                       } = do
  -- Lookup the type of function referred to by instance
  funtype <- queryFunIS fun funType
  -- Bindings of the function that we are instantiating
  funBinds <- queryNamedIS fun A.params

  instBinds <- case funtype of
        Complete -> do
          unless (length funBinds == length bindings) $ throwError "Parameter count mismatch"
          mapM mapBinding $ zip funBinds bindings
        Skeleton ->
          -- For non-synthesizable processes we simply create empty
          -- "placeholder" bindings and we don't care about param length mismatches
          return $ zip funBinds $ repeat EmptyVal
        Undecided ->
          throwError "Function of unknown kind encountered"

  inb' <- mapM checkBus inb
  outb' <- mapM checkBus outb
  -- We do explicit mappings of type parameters here to trigger warnings if we
  -- change the SMEIL AST
  return Instance { instParams = instBinds
                  , instName = name
                  , instFun = fun
                  , inBusses = inb'
                  , outBusses = outb'
                  }
  where
    -- Looks up binding and and maps it to function parameter if it is bound
    mapBinding :: (Ident, A.Binding) -> BindM (Ident, PrimVal)
    mapBinding (ident, A.Bound (Prim p)) = return (ident, p)
    mapBinding (_, A.Bound _) = throwError "Variable bound to non-primitive value"
    mapBinding _ = throwError "Instance parameter bound to free variable"

genPortMap :: Instance -> BindM InstPorts
genPortMap Instance { instFun = fun
                    , inBusses = ins
                    , outBusses = outs
                    } = return (fun, (ins, outs))
  -- TODO: Fix the following by introducing bus "traits" describing the ports of
  -- busses and their types
  -- do
  -- ins' <- mapM busKind ins
  -- outs' <- mapM busKind outs
  -- return (fun, (foldr (<>) [] ins',
  --               foldr (<>) [] outs'))
  -- where
  --   -- |Returns the ports of a bus
  --   busKind :: Ident -> BindM [Ident]
  --   busKind = flip queryBusDef busPorts


-- TODO: Compare bus "kinds", where the "kind" of a bus is the ports of a bus and their types.
unifyPorts :: [InstPorts] -> BindM [InstPorts]
-- FIXME: Is partial function (fst) usage safe here?
unifyPorts ps = trace (show ps) foldM merge [] (sortOn fst ps)
  where
    merge :: [InstPorts] -> InstPorts -> BindM [InstPorts]
    merge ips@(ip@(i, (ob, ib)):_) ip'@(i', (ob', ib'))
      -- | i == i'
      --   && ((ob /= ob')
      --   || (ib /= ib')) =  throwError $ unlines [ "Unable to unify ports in function instantiations"
      --                                           , "Instantiation of " ++ i ++ " with ports " ++ show ob' ++ show ib' ++ "conflicts with previous instantiation of " ++ i ++ " with ports " ++ show ob ++ show ib ]
      | ip == ip' = return ips
      | otherwise = return (ip':ips)
    merge [] ip = return [ip]

bindNetwork :: BindM Network
bindNetwork = do
  busList <- Map.elems <$> asks A.busses
  insts <- Map.elems <$> asks A.instances >>= mapM bindInstance
  instPorts <- mapM genPortMap insts >>= unifyPorts
  funs <- Map.elems <$> asks A.funStates >>= mapM (`withCurBlock` bindFunction instPorts)
  netName' <- asks A.netName
  return Network { functions = funs
                 , busses = busList
                 , instances = insts
                 , netName = netName'
                 }

bindPyMod :: A.AnState -> Either BindErr Network
bindPyMod s = runBindM s bindNetwork

-- We wish to map function parameters to VHDL generics. In order to make sure
-- that we have "safe passage" we need to check that all parameters of the
-- processes will be made bound by the parameters given at the time of process
-- instantiation.
