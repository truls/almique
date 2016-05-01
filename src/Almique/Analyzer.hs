{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, PatternSynonyms #-}

module Almique.Analyzer
       ( analyzePyMod
       , AnLog
       , AnError
       , AnState (..)
       , FunInterpState (..)
       , NewInst (..)
       , Binding (..)
       )
       where

import qualified Data.Map.Strict as Map
import Control.Monad.State.Lazy
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.Extra (allM)
import Data.Maybe

import Language.Python.Common as Py hiding (annot, (<>))

import qualified Language.SMEIL as SMEIL

--------------------------------------------------------------------------------
-- Analysis state data type definition
--------------------------------------------------------------------------------
type SMEIdent = String

-- TODO: Not sure we'll ever need more state information than  his?
--type NameState = Integer
type AnLog = [String]

data NewInst = NewInst { instBindings :: [ Binding ]
                       , inst :: SMEIL.Instance
                       }
             deriving Show

-- TODO: Give a more general definition for Bound
data Binding = Bound SMEIL.Expr
             | Free SMEIL.Expr
             | Binding Binding
             deriving (Show, Eq)

data AnState = AnState { currentBlock :: Maybe FunInterpState
                       , funStates :: Map.Map SMEIdent FunInterpState
                                      --, blocks :: Map Ident SMEIL.Function
                       , busses :: Map.Map SMEIdent SMEIL.Bus
                       , instances :: Map.Map SMEIdent NewInst
                       , classesSeen :: [String]
                       , netName :: SMEIdent
  --                     , nameState :: NameState
                       }
             deriving Show

type AnError = String

data BaseClass = Function | External | Network
               deriving (Show, Eq)

newState :: AnState
newState = AnState { currentBlock = Nothing
                   , funStates = mempty
                   , busses = mempty
                   , classesSeen = mempty
                   , instances = mempty
                   , netName = ""
--                   , nameState = 0
                   }

data FunInterpState = FunInterpState { mappedInBusses :: [SMEIdent]
                                     , mappedOutBusses :: [SMEIdent]
                                     , bindings :: Map.Map SMEIL.Variable Binding
                                     , params :: [SMEIdent]
                                     , blockFunction :: SMEIL.Function
                                     }
                    deriving (Show, Eq)
newFunInterpState :: FunInterpState
newFunInterpState = FunInterpState { mappedInBusses = mempty
                                   , mappedOutBusses = mempty
                                   , bindings = mempty
                                   , params = mempty
                                   , blockFunction = SMEIL.Function
                                     { SMEIL.funName = mempty
                                     , SMEIL.funInports = mempty
                                     , SMEIL.funOutports = mempty
                                     , SMEIL.funParams = mempty
                                     , SMEIL.locals = mempty
                                     , SMEIL.funBody = mempty
                                     , SMEIL.funType = SMEIL.Undecided
                                     }
                                   }


--------------------------------------------------------------------------------
-- Monad definitions and state manipulators
--------------------------------------------------------------------------------
newtype AnM a = AnM { unAnM :: ExceptT AnError
                                           (WriterT AnLog (State AnState)) a }
                    deriving ( Functor, Applicative, Monad
                             , MonadState AnState
                             , MonadError AnError
                             , MonadWriter AnLog )

runAnM :: AnState -> AnM () -> Either AnError (AnState, AnLog)
runAnM s m = let ((err, logged), fstate) = runState
                       (runWriterT $ runExceptT $ unAnM m) s
                 in
                   case err of
                     Left e -> Left e
                     Right () -> Right (fstate, logged)


setNetName :: SMEIdent -> AnM ()
setNetName n = modify (\s -> s { netName = n })

-- | Return a new globally unique random name
-- getName :: AnM String
-- getName = do
--   num <- gets nameState
--   modify (\s -> s { nameState = num + 1 } )
--   return $ "tmp_" ++ show num

addBus :: SMEIdent -> SMEIL.Bus -> AnM ()
addBus i b = modify (\s -> s { busses = Map.insert i b $ busses s } )

addInstance :: SMEIdent -> NewInst -> AnM ()
addInstance i b = modify (\s -> s { instances = Map.insert i b $ instances s } )

modifyIS :: (FunInterpState -> FunInterpState) -> AnM ()
modifyIS f = do
  -- FIXME: Fail somehow if this function is called when currentBlock is nothing
  curBlock <- isNothing <$> gets currentBlock
  unless curBlock ( modify (\s -> s { currentBlock = f <$> currentBlock s }))

queryIS :: (FunInterpState -> a) -> (a -> b) -> AnM b
queryIS f t = do
  hasCurBlock <- isNothing <$> gets currentBlock
  when hasCurBlock $ throwError "Internal compiler error: Tried querying and empty block"
  (Just curBlock) <- gets currentBlock
  return $ t $ f curBlock

addOutbusIS :: [SMEIdent] -> AnM ()
addOutbusIS i = modifyIS (\s -> s { mappedOutBusses = mappedOutBusses s <> i })

addInbusIS :: [SMEIdent] -> AnM ()
addInbusIS i = modifyIS (\s -> s { mappedInBusses = mappedInBusses s <> i })

addBindingIS :: SMEIL.Variable -> Binding -> AnM ()
addBindingIS k v = modifyIS (\s -> s { bindings = Map.insert k v $ bindings s })

getBindingIS :: SMEIL.Variable -> AnM (Maybe Binding)
getBindingIS i = queryIS bindings (Map.lookup i)

addParamsIS :: [SMEIdent] -> AnM ()
addParamsIS i = modifyIS (\s -> s { params = params s <> i } )

isParam :: SMEIdent -> AnM Bool
isParam v = queryIS params (\s -> v `elem` s)

-- isBound :: SMEIdent -> AnM Bool
-- isBound v = queryIS bindings (\s -> not $ isNothing $ lookup v s)

setFunInterpState :: FunInterpState -> AnM ()
setFunInterpState i = modify (\s -> s { currentBlock = Just i })

addFunInterpState :: FunInterpState -> AnM ()
addFunInterpState i = modify (\s -> s { currentBlock = Just i })

closeFunInterpState :: AnM ()
closeFunInterpState = do
  ostate <- gets currentBlock
  case ostate of
    Just s'@FunInterpState { blockFunction = SMEIL.Function { SMEIL.funName = name } } ->
      modify ( \s -> s { currentBlock = Nothing
                       , funStates = Map.insert name s' $ funStates s
                       } )
    Nothing -> throwError "clearFunInterpState called on empty state"

clearFunInterpState :: AnM ()
clearFunInterpState = modify (\s -> s { currentBlock = Nothing })

modifyCFun :: (SMEIL.Function -> SMEIL.Function) -> AnM ()
modifyCFun f = modifyIS (\s -> s { blockFunction = f $ blockFunction s })

queryCFun :: (SMEIL.Function -> a) -> (a -> b) -> AnM b
queryCFun f t = do
  curfun <- queryIS blockFunction id
  return $ t $ f curfun

setFunName :: SMEIdent -> AnM ()
setFunName i = modifyCFun (\s -> s { SMEIL.funName = i } )

setFunType :: BaseClass -> AnM ()
setFunType c = modifyCFun (\s -> s { SMEIL.funType = mapClass c } )
  where
   -- TODO: Avoid this silly mapping by using SMEIL types all over
   mapClass Function = SMEIL.Complete
   mapClass External = SMEIL.Skeleton
   mapClass Network = SMEIL.Undecided

addFunInport :: (SMEIL.Ident, SMEIL.Ident) -> AnM ()
addFunInport i = modifyCFun (\s -> s { SMEIL.funInports = SMEIL.funInports s <> pure i })

addFunOutport :: (SMEIL.Ident, SMEIL.Ident) -> AnM ()
addFunOutport i = modifyCFun (\s -> s { SMEIL.funOutports = SMEIL.funOutports s <> pure i})

addFunLocal :: SMEIL.Decl -> AnM ()
addFunLocal d = modifyCFun (\s -> s { SMEIL.locals = SMEIL.locals s <> pure d })

addFunStmt :: SMEIL.Stmt -> AnM ()
addFunStmt d = modifyCFun (\s -> s { SMEIL.funBody = SMEIL.funBody s <> SMEIL.Stmts (pure d) })

-- |Execute action x in a new context and merge context into AnState
withNewBlockContext :: AnM () -> AnM () -> AnM ()
withNewBlockContext f merger = do
  curState <- isNothing <$> gets currentBlock
  when curState (setFunInterpState newFunInterpState
                  >> f
                  >> merger)

--------------------------------------------------------------------------------
-- Pattern definitions
--------------------------------------------------------------------------------

pattern PVarIdent i <- Var { var_ident = Ident { ident_string = i } }
pattern PIntLit i <- Int { int_value = i }
pattern PIdent n <- Ident { ident_string = n }
pattern PString1 s <- Strings { strings_strings = [s] }
pattern PListEls l <- List { list_exprs = l }
pattern PArgExpr e <- ArgExpr { arg_expr = e }
pattern PAssign to expr <- Assign { assign_to = [ to ]
                                  , assign_expr = expr
                                  }
pattern PClassAssign to ident args <- PAssign (PVarIdent to) Call { call_fun = ident
                                                                 , call_args = args
                                                                 }
pattern PSelfFunCall fun args <- StmtExpr { stmt_expr =
                                           Call { call_fun =
                                                   Dot { dot_expr = PVarIdent "self"
                                                       , dot_attribute = PIdent fun
                                                       }
                                                 , call_args = args
                                                 }
                                          }
pattern PSelfDot dest <- Dot { dot_expr = PVarIdent "self"
                             , dot_attribute = PIdent dest
                             }
pattern PSelfAssign dest val <- PAssign (PSelfDot dest) val
pattern PExprInt n = SMEIL.Prim (SMEIL.Num (SMEIL.SMEInt n))
pattern PExprFloat n = SMEIL.Prim (SMEIL.Num (SMEIL.SMEFloat n))
pattern PExprBool n = SMEIL.Prim (SMEIL.Bool n)
pattern PBoundNum n = Bound (PExprInt n)
pattern PFreeNamedVar v = Free (SMEIL.Var (SMEIL.NamedVar v))

--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

stringifyList :: Expr SrcSpan -> AnM [String]
stringifyList (PListEls els) = mapM el els
  where
    el :: Expr SrcSpan -> AnM String
    el (PString1 s) = return $ unquote s
    el (PVarIdent i) = return i
    el _ = throwError "Only lists consisting entirely of variable names, strings or numbers are supported"
stringifyList _ = throwError "Not a list"

stringifyArgList :: [Argument SrcSpan] -> AnM [String]
stringifyArgList els = do
  (list, annots) <- mapAndUnzipM unArgExpr els
  stringifyList List { list_exprs = list
                     , expr_annot = foldr combineSrcSpans SpanEmpty annots }
  where
    unArgExpr :: Argument SrcSpan -> AnM (Expr SrcSpan, SrcSpan)
    unArgExpr ArgExpr { arg_expr = e, arg_annot = s } = return (e, s)
    unArgExpr _ = throwError "Only simple arguments are currently supported"

stringifyParamList :: [Parameter SrcSpan] -> AnM [String]
stringifyParamList = mapM unParam
  where
    unParam :: Parameter SrcSpan -> AnM String
    unParam Param { param_name = PIdent i } = return i
    unParam par =  throwError ("Unsupported parameter" ++ show par)

-- TODO: Only remove quotation marks at start or end of string
unquote :: String -> String
unquote = filter (`notElem` ['"', '\''])
funName :: String -> Statement SrcSpan -> Bool

funName names Fun {fun_name = PIdent name} = name == names
funName _ _ = False

busDef :: SMEIdent -> SMEIL.Variable
busDef i = SMEIL.BusVar i ""

--------------------------------------------------------------------------------
-- Simple Python -> SMEIL type mappings
--------------------------------------------------------------------------------

mapOp :: Op SrcSpan -> AnM SMEIL.BinOps
mapOp And {} = return SMEIL.AndOp
mapOp Or {} = return SMEIL.AndOp
mapOp Plus {} = return SMEIL.PlusOp
mapOp Minus {} = return SMEIL.MinusOp
mapOp Multiply {} = return SMEIL.MulOp
mapOp Equality {} = return SMEIL.EqOp
mapOp NotEquals {} = return SMEIL.NeqOp
mapOp Divide {} = return SMEIL.DivOp
mapOp BinaryOr {} = return SMEIL.OrOp
mapOp Xor {} = return SMEIL.XorOp
mapOp BinaryAnd {} = return SMEIL.AndOp
mapOp ShiftLeft {} = return SMEIL.SLOp
mapOp ShiftRight {} = return SMEIL.SROp
mapOp _ = throwError "Unsupported operator"

mapUnOp :: Op SrcSpan -> AnM SMEIL.UnOps
mapUnOp Not {} = return SMEIL.NotOp
mapUnOp _ = throwError "Unsupported unary operator"

mapAssignOp :: AssignOp SrcSpan -> AnM (Op SrcSpan)
mapAssignOp PlusAssign {assignOp_annot = an} = return Plus {op_annot = an}
mapAssignOp MinusAssign {assignOp_annot = an} = return Minus {op_annot = an}
mapAssignOp MultAssign {assignOp_annot = an} = return Multiply {op_annot = an}
mapAssignOp LeftShiftAssign {assignOp_annot = an} = return ShiftLeft {op_annot = an}
mapAssignOp RightShiftAssign {assignOp_annot = an} = return ShiftRight {op_annot = an}
-- FIXME: Print nicer error message including annotation
mapAssignOp _ = throwError "Operator not supported"

mapBool :: Bool -> SMEIL.SMEBool
mapBool True = SMEIL.SMETrue
mapBool False = SMEIL.SMEFalse

--------------------------------------------------------------------------------
-- Interpretation definitions
--------------------------------------------------------------------------------

classifyClass :: [Argument SrcSpan] -> Maybe BaseClass
classifyClass [ArgExpr {arg_expr = PVarIdent arg}]
  | arg == "External" = Just External
  | arg == "Function" = Just Function
  | arg == "Network" = Just Network
  | otherwise = Nothing
classifyClass _ = Nothing

-- |Generates code for certain toplevel classes
genStatement :: Py.Statement SrcSpan -> AnM ()
genStatement cls@Class { class_name = Ident { ident_string = name }
                       , class_args = args
                       , class_body = _body
                       } = case classifyClass args of
  Just t@Function -> genFun t
  Just t@External -> genFun t
  Just Network -> setNetName name >>
    withNewBlockContext (mapBlockStms (genNetwork name) cls) clearFunInterpState
  Nothing -> do
    -- FIXME Log that a class was ignored somewhere (and possibly the reason)
    tell ["Ignoring class " ++ name ++ " due to unsupported class inheritance. i.e. we have no fucking idea what the fuck it's doing (yet)"]
    return ()
  where
    genFun t = withNewBlockContext (mapBlockStms (genFunction t name) cls) mergeState

    mergeState :: AnM ()
    mergeState = closeFunInterpState
genStatement _ = tell ["Ignoring unsupported top-level statement"]

mapBlockStms :: (Statement SrcSpan -> AnM ()) -> Statement SrcSpan -> AnM ()
mapBlockStms f Fun {fun_body = body} = mapM_ f body
mapBlockStms f Class {class_body = body} = mapM_ f body
mapBlockStms f s = do
  -- FIXME: Does this makes sense? Isn't it an error?
  tell ["WARNING: called mapBlockStms on statements other fhan Fun or Class"]
  f s

genFunction :: BaseClass -> String -> Statement SrcSpan -> AnM ()
genFunction t name s
  -- TODO: Make it an error if not both setup and run are present in a class
  | funName "setup" s = setFunName name >> setFunType t >> setupParams s >> mapBlockStms genSetup s
  | funName "run" s = when (t == Function) (mapBlockStms genRun s)
  | otherwise = tell ["Ignoring unknown statement in function"]
  where

    setupParams :: Statement SrcSpan -> AnM ()
    setupParams Fun { fun_name = PIdent "setup"
                    , fun_args = (_self:_inp:_outp:rest)} = stringifyParamList rest >>= addParamsIS
    setupParams Fun { fun_name = PIdent "setup" } =
      throwError "Setup function must contain at least three parameters: self, inports and outports"
    setupParams _ = throwError "Got passed something that wasn't a function"

    genSetup :: Statement SrcSpan -> AnM ()
    -- TODO: Check that first arg of map_{ins,outs} functions refers to
    -- parameters of the setup function
    genSetup (PSelfFunCall "map_ins" (_:args) ) = do
      argList <- stringifyArgList args
      addInbusIS argList
    genSetup (PSelfFunCall "map_outs" (_:args) ) = do
      argList <- stringifyArgList args
      addOutbusIS argList
    genSetup (PSelfAssign dest (PVarIdent symb)) = do
      param <- isParam symb
        -- TODO: Lookup variable binding in either list of bound variables or
        -- parameters
      if param then
        void $ addBindingIS (SMEIL.NamedVar dest) $ PFreeNamedVar symb
         else
        -- TODO: Do something useful here
        void $ addBindingIS (SMEIL.NamedVar dest) $ PFreeNamedVar $ symb ++ "_bound"
    genSetup (PSelfAssign dest (PIntLit i )) =
      addBindingIS (SMEIL.NamedVar dest) $ PBoundNum i
    genSetup _ = tell ["Ignoring statement in setup function"]

    genRun :: Statement SrcSpan -> AnM ()
    genRun a = genStm a >>= addFunStmt

genExpr :: Expr SrcSpan -> AnM SMEIL.Expr
genExpr (PIntLit n) = return $ PExprInt  n
-- TODO: Check if variable is defined and check if value is a bus or variable
genExpr Float { float_value = n } = return $ PExprFloat n
genExpr Bool { bool_value = b } = return $ PExprBool $ mapBool b
-- Bus assignment
genExpr Subscript { subscriptee = PSelfDot v
                  , subscript_expr = PString1 s } = return $ SMEIL.Var . SMEIL.BusVar v $ unquote s
genExpr BinaryOp { operator = op
                 , left_op_arg = l
                 , right_op_arg = r } =
  SMEIL.BinOp <$> mapOp op  <*> genExpr l <*> genExpr r
genExpr UnaryOp { operator = op
                , op_arg = arg } = SMEIL.UnOp <$> mapUnOp op <*> genExpr arg
genExpr (PSelfDot v) = SMEIL.Var . SMEIL.NamedVar <$> pure v
genExpr Paren { paren_expr = e } = SMEIL.Paren <$> genExpr e
genExpr (PVarIdent v) = SMEIL.Var . SMEIL.NamedVar <$> pure v
-- TODO: Figure out what to do with parsing of assignment to busses
genExpr e = tell ["Unsupported expression " ++ show e] >> return SMEIL.NopExpr

genStm :: Statement SrcSpan -> AnM SMEIL.Stmt
--genStm (PSelfAssign ident expr) = pure (SMEIL.Assign ident) <*> genExpr expr
genStm Assign { assign_to = [to]
              , assign_expr = expr } = do
  assignTo <- genExpr to
  case assignTo of
    -- TODO Lookup variable in binding list and see if it is already defined
    --      if not, add it to the list of bindings or else redefine
    SMEIL.Var i -> SMEIL.Assign i <$> genExpr expr
    _ -> throwError $ "Assign to expression not supported (not reducible to Var expression)" ++ show to
genStm Conditional { cond_guards = guards
                   , cond_else = condElse
                   } = SMEIL.Cond <$> mapM genGuards guards <*>
  (SMEIL.Stmts <$> mapM genStm condElse)
      where
        genGuards :: (Expr SrcSpan, Suite SrcSpan) -> AnM (SMEIL.Expr, SMEIL.Stmts)
        genGuards (e, s) = (,) <$> genExpr e <*> (SMEIL.Stmts <$> mapM genStm s)
genStm AugmentedAssign { aug_assign_to = to
                       , aug_assign_op = op
                       , aug_assign_expr = expr
                       , stmt_annot = annot
                       } = do
  newop <- mapAssignOp op
  genStm Assign { assign_to = [to]
                , assign_expr = BinaryOp { operator = newop
                                         , left_op_arg = to
                                         , right_op_arg = expr
                                         , expr_annot = annot }
                , stmt_annot = annot
                }
genStm _ = tell ["Unsupported statement"] >> return SMEIL.NopStmt

genNetwork :: String -> Statement SrcSpan -> AnM ()
genNetwork name stm = when (funName "wire" stm) (mapBlockStms networkDef stm)
  where
    mapType :: String -> AnM SMEIL.DType
    mapType "int" = return SMEIL.IntType
    mapType "float" = return SMEIL.FloatType
    mapType s = throwError $ "Unsupported bus type " ++ s

    networkDef :: Statement SrcSpan -> AnM ()
    networkDef (PClassAssign varName (PVarIdent "Bus")
                [ ArgExpr { arg_expr = PString1 bname }
                , ArgExpr { arg_expr = parlist }
                , ArgExpr { arg_expr = PVarIdent btype }
                ]
               ) = do
      smetype <- mapType btype
      ports <- stringifyList parlist
      addBindingIS (SMEIL.BusVar varName "") (Bound SMEIL.NopExpr)
      addBus varName SMEIL.Bus { SMEIL.busName = unquote bname
                               , SMEIL.busDtype = smetype
                               , SMEIL.busPorts = ports
                               }
    networkDef (PClassAssign varName (PVarIdent instof)
                ( ArgExpr { arg_expr = PString1 iname }
                  : ArgExpr { arg_expr = inbusses }
                  : ArgExpr { arg_expr = outbusses }
                  -- FIXME: Handle arbitrary parameters
                  : rest )) = do
      inbuss <- stringifyList inbusses
      outbuss <- stringifyList outbusses
      bbound <- bussesBound (outbuss ++ inbuss)
      unless bbound $
        throwError $ "One of the following busses were undefined " ++ show (inbuss ++ outbuss)
      params' <- stringifyArgList rest
      instBinds <- mapM evalBinding rest
      -- FIXME: We can probably get rid of the NewInst wrapping of Instance now
      addInstance varName NewInst { instBindings = instBinds
                                  , inst  = SMEIL.Instance { SMEIL.instName = unquote iname
                                                           , SMEIL.instFun = instof
                                                           , SMEIL.inBusses = inbuss
                                                           , SMEIL.outBusses = outbuss
                                                           , SMEIL.instParams =
                                                             zip params' $ repeat SMEIL.EmptyVal
                                                           }
                                  }
        where
          evalBinding :: Argument SrcSpan -> AnM Binding
          evalBinding (ArgExpr p _) = do
            expr <- genExpr p
            case expr of
              n@(SMEIL.Prim (SMEIL.Num _)) -> return $ Bound n
              i@(SMEIL.Var v@(SMEIL.NamedVar _n)) -> do
                binding <- getBindingIS v
                case binding of
                  Just (Bound e) -> return $ Bound e
                  Just free -> return $ Binding free
                  Nothing -> return $ Free i
              _ -> tell ["Invalid expression in bus instantiating 1" ++ show p]
                >> return (Free SMEIL.NopExpr)
          evalBinding _ = tell ["Invalid expression in bus instantiation 2"]
            >> return (Free SMEIL.NopExpr)

          bussesBound :: [SMEIdent] -> AnM Bool
          bussesBound = allM isBoundBus
            where
              isBoundBus :: SMEIdent -> AnM Bool
              isBoundBus i = do
                binding <- getBindingIS $ busDef i
                return $ isJust binding

    networkDef Fun {fun_name = _n}  = tell ["there shouldn't be a function here "]
    networkDef s = do
      -- FIXME: I don't like this
      stat <- genStm s `catchError` (\_ -> return SMEIL.NopStmt)
      case stat of
        (SMEIL.Assign i n@(SMEIL.Var (SMEIL.NamedVar _))) -> addBindingIS i (Free n)
        (SMEIL.Assign i n@(SMEIL.Prim (SMEIL.Num _))) -> addBindingIS i (Bound n)
        (SMEIL.Assign i SMEIL.NopExpr) -> addBindingIS i (Free SMEIL.NopExpr)
        _ -> tell ["Ignoring unknown statement in busdef" ++ show stat]

genModule :: Module SrcSpan -> AnM ()
genModule (Module m) = mapM_ genStatement m

analyzePyMod :: ModuleSpan -> Either AnError (AnState, AnLog)
analyzePyMod pymod = runAnM newState (genModule pymod)
