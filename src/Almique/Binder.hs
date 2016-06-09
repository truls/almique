{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Almique.Binder
       ( bindPyMod
       , BindErr
       ) where

import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.Map.Strict as Map hiding (map)
import qualified Data.Set as Set
import Data.Maybe (isNothing, fromMaybe, catMaybes)
import Data.List
import Data.Monoid

import Language.SMEIL
import qualified Almique.Analyzer as A

import Debug.Trace

type InstPorts = (Ident, ([Ident], [Ident]))
type InstParams = (Ident, (DType, Ident))
--type VarTypes = Set.Set Variable
type VarTypes = Map.Map Ident DType

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
resolveFree (NamedVar _ n) = queryIS A.params $ elem n
resolveFree _ = throwError "resolveFree: Only NamedVar variables can be free?"

-- |Lookup variable binding. If bound, return corresponding expression. If free,
-- try resolving the binding as a parameter and return Nothing if unsuccessful
lookupBinding :: Variable -> BindM (Maybe (DType, Expr))
lookupBinding v = do
  binding <- queryIS A.bindings (Map.lookup v)
  case binding of
    Just (A.Bound t e) -> return $ Just (t, e)
    Just (A.Free t e@(Var var)) -> do
      resolved <- resolveFree var
      if resolved then
        return $ Just (t, e)
        else
        return Nothing
    -- TODO: Check that other expressions can be fully evaluated at this point
    Just (A.Free _t _e) -> return Nothing
    _ -> return Nothing

withCurBlock :: A.FunInterpState -> BindM a -> BindM a
withCurBlock s = local (\c -> c { A.currentBlock = Just s })

queryBusDef :: Ident -> (Bus -> a) -> BindM a
queryBusDef i f = do
  -- FIXME: Mostly a duplicate of queryNamedIS. Consolidate!
  el <- asks A.busses
  case Map.lookup i el of
    Just v -> return $ f v
    Nothing -> case busByName $ Map.elems el of
      Just v -> return $ f v
      Nothing -> throwError $ "Reference to undefined bus " ++ i
    where
      -- HACK: The map of busses maps from the variable name that busses were
      -- originally assigned to in the python code rather than the actual name
      -- of the bus. Therefore, we query first varNames and then their real
      -- names
      busByName :: [Bus] -> Maybe Bus
      busByName (b:bs) = if busName b == i then
                             Just b
                           else
                             busByName bs
      busByName [] = Nothing

-- FIXME: Querying a bus just to see if queryBusDef will throw an
-- error... Is this bad?
-- Replaced by resolveBusName in bindInstance
--checkBus :: Ident -> BindM Ident
--checkBus b = queryBusDef b id >> return b

-- Generate a set of all names that occur in process code. Used for propagating
-- types down in the AST
-- Consider the following:
-- 1) Maybe this list should be moved into the BindM monad so that we don't have
-- to explicitly pass it around from checkStmts
-- 2) Maybe a map from names to types is better than a set of Variables?
genNameSet :: Ident -> [(Ident, Ident)] -> [Decl] -> [Decl] -> BindM VarTypes
genNameSet fName busNameMap localList params = do
  busList <- concat <$> mapM busVars busNameMap
  let decls = trace (show localList) $ map declVars localList
  let params' = map paramVars params
  return $ Map.fromList busList <> Map.fromList decls <> Map.fromList params'
  where
    busVars :: (Ident, Ident) -> BindM [(Ident, DType)]
    busVars (l, n) = do
      bus <- queryBusDef n id
      let t = busDtype bus
      return [(l ++ "_" ++ p, t) | p <- busPorts bus ]

    declVars :: Decl -> (Ident, DType)
    declVars (Decl var _ _) = (nameOf var, typeOf var)

    paramVars :: Decl -> (Ident, DType)
    -- TODO: Only supporting integer parameters for now. Extend this to
    -- inferring parameter types based on parameter values in instantiations
    paramVars (Decl (NamedVar AnyType n) _ _) = (n, IntType 32)
    -- FIXME: Do something better here
    paramVars (Decl v _ _) = (nameOf v, typeOf v)

-- | For each function, check that all variables in function bodies are defined
-- verify that variable kinds are correct. Furthermore, we should infer the
-- the variable kinds being used
bindFunction :: [InstPorts] -> BindM Function
bindFunction ps = do
  -- FIXME: Most of these mappings (and by extension the entire FunInterpState
  -- type) ended up being rather pointless.
  funName' <- queryFunCurIS funName
  funType' <- queryFunCurIS funType
  let (inPortTypes, outPortTypes) = getPortMap funName'
  inPorts <- queryIS A.mappedInBusses (`zip` inPortTypes) -- >>= mapM checkBus
  outPorts <- queryIS A.mappedOutBusses (`zip` outPortTypes) -- >>= mapM checkBus
  funParams' <- map mapFunParam <$> queryIS A.params id -- >>= genFunParam
  allLocals <- queryIS A.bindings Map.toList >>= mapM (mapBinding [])
  nameSet <- genNameSet funName' (inPorts ++ outPorts)
    (catMaybes allLocals) funParams'
  (funBody', funVars) <- queryFunCurIS funBody >>= checkStmts nameSet
  --funBody'' <- queryFunCurIS funBody

  -- FIXME: Using toList feels like a bad idea
  locals' <- queryIS A.bindings Map.toList >>= mapM (mapBinding funVars)
  return Function { funName = funName'
                  , funInports = inPorts
                  , funOutports = outPorts
                  , funParams = funParams'
                  --, locals = concat $ sequence locals'
                  , locals = foldr (\a b -> b ++ fromMaybe [] ((: []) <$> a)) [] locals'
                  , funBody = funBody'
                  , funType = funType'
                  }
    where
      mapFunParam :: Ident -> Decl
      -- FIXME: Kind of useless right now. The Decl in order to enable future
      -- support for default values
      mapFunParam a = (Decl $ NamedVar AnyType a) AnyType Nothing

      getPortMap :: Ident -> ([Ident], [Ident])
      getPortMap i = fromMaybe ([], []) $ lookup i ps


      -- FIXME: This function doesn't really make sense:
      --  1) At this point. a binding cannot be anything else than a NamedVar
      --  2) Bindings declared in this way must have an initial value, i.e. we
      --  we cannot have a Decl in locals whose value expr evaluates to
      --  anything else that a PrimVal
      --  3) Why do we pass a tuple as the second argument if we only use the
      --  first component of it
      --
      -- The only purpose of this function is to generate the local clause of
      -- SMEIL functions from the bindings generated in the analysis phase which
      -- are bound to a primval and setting constness.
      -- Hacking a solution for now, but review this at some point.
      mapBinding :: [Variable] -> (Variable, A.Binding) -> BindM (Maybe Decl)
      mapBinding vs (i, _b) = do
        binding <- lookupBinding i
--        let free = fromMaybe False (snd <$> binding)
        case binding of
          -- The hack is here
          (Just (_, Var{})) -> return Nothing
          (Just (t, v)) -> return $ case i of
                                   BusVar _ _ _ -> Just $ Decl i t $ Just v
                                   ConstVar _ _ -> Just $ Decl i t $ Just v
                                   ParamVar _ _ -> Just $ Decl i t $ Just v
                                   (NamedVar ty n) ->
                                     if i `notElem` vs then
                                       Just $ Decl (ConstVar ty n) t $ Just v
                                       else
                                          Just $ Decl i t $ Just v
          Nothing -> queryFunCurIS funType >>= \case
            Skeleton -> return $ Just $ Decl i AnyType Nothing
            _ -> throwError $ "Reference to free variable " ++ show i


-- Static checking of SMEIL

-- | Map a function over Stmts and accumulate the transformed statements and
-- the the list of variables being assigned to (=modified) by the
-- statements. We use this to infer variable constness.
checkStmts :: VarTypes -> Stmts -> BindM (Stmts, [Variable])
checkStmts vs (Stmts s) =
  mapM (checkStmt vs) s >>= foldM (\(stmts, vars) (stmt, var) ->
                                      return ( stmts <> Stmts [stmt]
                                             , vars <> var))
                            (mempty, mempty)

checkStmt :: VarTypes -> Stmt -> BindM (Stmt, [Variable])
checkStmt vts (Cond cs e) = do
  let (es, ss) = unzip cs
  es' <- mapM (checkExpr vts) es
  (ss', vs) <- mapAndUnzipM (checkStmts vts) ss
  (e', ve) <- checkStmts vts e
  return (Cond (zip es' ss') e', ve ++ concat vs)
checkStmt vs (Assign v e) = do
  e' <- checkExpr vs e
  v' <- checkVar vs v
  return (Assign v' e', [v])
checkStmt _ s = return (s, [])

checkExpr :: VarTypes -> Expr -> BindM Expr
checkExpr vs BinOp { op = o
                   , left = l
                   , right = r } = BinOp <$> pure o <*> checkExpr vs l <*> checkExpr vs r
checkExpr vs UnOp { unOp = o
                  , unOpVal = v } = UnOp <$> pure o <*> checkExpr vs v
checkExpr vs (Paren e) = Paren <$> checkExpr vs e
checkExpr vs (Var v) = Var <$> checkVar vs v
checkExpr _ p@Prim{} = return p
checkExpr _ n@NopExpr = return n

setType :: VarTypes -> Variable -> BindM Variable
setType vs v = do
  let varType = trace (show vs) Map.lookup (nameOf v) vs
  varType' <- trace (show varType) $ case varType of
                Just t -> return t
                Nothing -> throwError $ "Undefined variable  " ++ show vs ++ " " ++  show v
  case v of
    (ConstVar _ n) -> return $ ConstVar varType' n
    (ParamVar _ n) -> return $ ParamVar varType' n
    (NamedVar _ n) -> return $ NamedVar varType' n
    (BusVar _ n n') -> return $ BusVar varType' n n'

{- TODO:
- Check variable bindings. If variable is resolved to a function
  parameter, replace occurrences of the variable with the name of the
  parameter. We do this, because parameters will be mapped to VHDL generics.
- Infer variable constness. Variables that are never assigned to and have a default
  value are inferred as constants. To do this, keep the kind of each variable
  somewhere, maybe defaulting to constant and then change to regular variables
  if assigned to or updated.
-}
checkVar :: VarTypes -> Variable -> BindM Variable
checkVar vs v@(NamedVar _ _n) = setType vs =<< do
  binding <- lookupBinding v
  case binding of
    Just (_t, Var nv@(NamedVar _ n)) -> do
      isParam <- resolveFree nv
      if isParam then
        return $ ParamVar (typeOf nv) n
        else
        return v
    Just _ -> return v
    Nothing -> return v
checkVar vs v = setType vs v

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

  inb' <- mapM resolveBusName inb
  outb' <- mapM resolveBusName outb
  -- We do explicit mappings of type parameters here to trigger warnings if we
  -- change the SMEIL AST
  return Instance { instParams = instBinds
                  , instName = name
                  , instFun = fun
                  , inBusses = inb'
                  , outBusses = outb'
                  }
  where
    --inferPrim :: PrimVal
    -- Looks up binding and and maps it to function parameter if it is bound
    mapBinding :: (Ident, A.Binding) -> BindM (Ident, PrimVal)
    -- FIXME: Maybe do something about the type here
    mapBinding (ident, A.Bound _type (Prim p)) = return (ident, p)
    mapBinding (_, A.Bound _type _) = throwError "Variable bound to non-primitive value"
    mapBinding v = throwError $ "Instance parameter bound to free variable " ++ show v

    resolveBusName :: Ident -> BindM Ident
    resolveBusName i = queryBusDef i busName

-- genParamTYpe :: Instance -> Bind InstParams
-- genparamTYpe Instance { instParmas = params

genPortMap :: Instance -> InstPorts
genPortMap Instance { instFun = fun
                    , inBusses = ins
                    , outBusses = outs
                    } = (fun, (ins, outs))

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
unifyPorts ps = foldM merge [] (sortOn fst ps)
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
  instPorts <- unifyPorts $ map genPortMap insts
  funs <- Map.elems <$> asks A.funStates >>= mapM (`withCurBlock` bindFunction instPorts)
  netName' <- asks A.netName
  return Network { functions = funs
                 , busses = busList
                 , instances = insts
                 , netName = netName'
                 }

bindPyMod :: A.AnState -> Either BindErr Network
bindPyMod s = runBindM s bindNetwork
