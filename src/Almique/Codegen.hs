{-# LANGUAGE GeneralizedNewtypeDeriving, NamedFieldPuns, PatternSynonyms #-}
--{-# LANGUAGE ViewPatterns #-}
--{-# LANGUAGE RecordWildCards #-}

module Almique.Codegen
       ( pyToSMEIL
       , Log
       , GenError
       , CodegenState
       )
       where

import qualified Data.Map.Strict as Map
import Control.Monad.State.Lazy
import Control.Monad.Except
import Control.Monad.Writer
import Data.Maybe

import Language.Python.Common as Py hiding (annot)

import qualified Language.SMEIL as SMEIL

--------------------------------------------------------------------------------
-- Interpretation state datatypes
--------------------------------------------------------------------------------
type SMEIdent = String

-- TODO: Not sure we'll ever need more state information than  his?
type NameState = Integer
type Log = [String]

data CodegenState = CodegenState { currentBlock :: Maybe FunInterpState
                                 , functions :: [SMEIL.Function]
                                 --, blocks :: Map Ident SMEIL.Function
                                 , busses :: Map.Map SMEIdent SMEIL.Bus
                                 , instances :: Map.Map SMEIdent SMEIL.Instance
                                 , classesSeen :: [String]
                                 , nameState :: NameState
                                 }
                    deriving Show

type GenError = String

data BaseClass = Function | External | Network

newState :: CodegenState
newState = CodegenState { currentBlock = Nothing
                        , functions = mempty
                        , busses = mempty
                        , classesSeen = mempty
                        , instances = mempty
                        , nameState = 0
                        }


-- data Binding = Resolved SMEIL.SMENum
--              | Unresolved SMEIdent
--              deriving (Show, Eq)
-- TODO: Define bound as something useful
data Binding = Bound SMEIL.SMENum
--data Binding = Bound SMEIdent
             | Free SMEIdent
             | Binding
             deriving (Show, Eq)

data FunInterpState = FunInterpState { mappedInBusses :: [SMEIdent]
                                     , mappedOutBusses :: [SMEIdent]
                                     , bindings :: Map.Map SMEIdent Binding
                                     , params :: [SMEIdent]
                                     , blockName :: SMEIdent
                                     , blockFunction :: SMEIL.Function
                                     }
                      deriving (Show, Eq)
newFunInterpState :: FunInterpState
newFunInterpState = FunInterpState { mappedInBusses = mempty
                                   , mappedOutBusses = mempty
                                   , bindings = mempty
                                   , params = mempty
                                   , blockName = mempty
                                   , blockFunction = SMEIL.Function
                                     { SMEIL.funName = mempty
                                     , SMEIL.funInports = mempty
                                     , SMEIL.funOutports = mempty
                                     , SMEIL.locals = mempty
                                     , SMEIL.funBody = mempty
                                     }
                                   }

--------------------------------------------------------------------------------
-- Pattern definitions
--------------------------------------------------------------------------------
pattern VarIdent i <- Var { var_ident = Ident { ident_string = i } }
pattern PIntLit i <- Int { int_value = i }
pattern PIdent n <- Ident { ident_string = n }
pattern PString1 s <- Strings { strings_strings = [s] }
pattern PListEls l <- List { list_exprs = l }
pattern PArgExpr e <- ArgExpr { arg_expr = e }
pattern PAssign to expr <- Assign { assign_to = [ to ]
                                  , assign_expr = expr
                                  }
pattern PClassAssign to ident args <- PAssign (VarIdent to) Call { call_fun = ident
                                                                 , call_args = args
                                                                 }
pattern PSelfFunCall fun args <- StmtExpr { stmt_expr =
                                           Call { call_fun =
                                                   Dot { dot_expr = VarIdent "self"
                                                       , dot_attribute = PIdent fun
                                                       }
                                                 , call_args = args
                                                 }
                                          }
pattern PSelfDot dest <- Dot { dot_expr = VarIdent "self"
                             , dot_attribute = PIdent dest
                             }
pattern PSelfAssign dest val <- PAssign (PSelfDot dest) val

--------------------------------------------------------------------------------
-- Monad definitions and state manipulators
--------------------------------------------------------------------------------
newtype Codegen a = Codegen { unCodegen :: ExceptT GenError
                                           (WriterT Log (State CodegenState)) a }
                    deriving ( Functor, Applicative, Monad
                             , MonadState CodegenState
                             , MonadError GenError
                             , MonadWriter Log )

runCodegen :: CodegenState -> Codegen () -> Either GenError (CodegenState, Log)
runCodegen s m = let ((err, logged), fstate) = runState
                       (runWriterT $ runExceptT $ unCodegen m) s
                 in
                   case err of
                     Left e -> Left e
                     Right () -> Right (fstate, logged)


addClass :: String -> CodegenState -> CodegenState
addClass name s = s { classesSeen = classesSeen s `mappend` [name] }

-- | Return a new globally unique random name
getName :: Codegen String
getName = do
  num <- gets nameState
  modify (\s -> s { nameState = num + 1 } )
  return $ "tmp_" ++ show num

addBus :: SMEIdent -> SMEIL.Bus -> Codegen ()
addBus i b = modify (\s -> s { busses = Map.insert i b $ busses s } )

addInstance :: SMEIdent -> SMEIL.Instance -> Codegen ()
addInstance i b = modify (\s -> s { instances = Map.insert i b $ instances s } )

modifyIS :: (FunInterpState -> FunInterpState) -> Codegen ()
modifyIS f = do
  -- FIXME: Fail somehow if this function is called when currentBlock is nothing
  curBlock <- isNothing <$> gets currentBlock
  unless curBlock ( modify (\s -> s { currentBlock = f <$> currentBlock s }))

addOutbusIS :: [SMEIdent] -> Codegen ()
addOutbusIS i = modifyIS (\s -> s { mappedOutBusses = mappedOutBusses s `mappend` i })

addInbusIS :: [SMEIdent] -> Codegen ()
addInbusIS i = modifyIS (\s -> s { mappedInBusses = mappedInBusses s `mappend` i })

addBindingIS :: SMEIdent -> Binding -> Codegen ()
addBindingIS k v = modifyIS (\s -> s { bindings = Map.insert k v $ bindings s })

addParamsIS :: [SMEIdent] -> Codegen ()
addParamsIS i = modifyIS (\s -> s { params = params s `mappend` i } )

queryIS :: (FunInterpState -> a) -> (a -> b) -> Codegen b
queryIS f t = do
  hasCurBlock <- isNothing <$> gets currentBlock
  when hasCurBlock $ throwError "Internal compiler error: Tried querying and empty block"
  (Just curBlock) <- gets currentBlock
  return $ t $ f curBlock

isParam :: SMEIdent -> Codegen Bool
isParam v = queryIS params (\s -> v `elem` s)

-- isBound :: SMEIdent -> Codegen Bool
-- isBound v = queryIS bindings (\s -> not $ isNothing $ lookup v s)

setFunInterpState :: FunInterpState -> Codegen ()
setFunInterpState i = modify (\s -> s { currentBlock = Just i })

clearFunInterpState :: Codegen ()
clearFunInterpState = modify (\s -> s { currentBlock = Nothing })

modifyCFun :: (SMEIL.Function -> SMEIL.Function) -> Codegen ()
modifyCFun f = modifyIS (\s -> s { blockFunction = f $ blockFunction s })

queryCFun :: (SMEIL.Function -> a) -> (a -> b) -> Codegen b
queryCFun f t = do
  curfun <- queryIS blockFunction id
  return $ t $ f curfun

setFunName :: SMEIdent -> Codegen ()
setFunName i = modifyCFun (\s -> s { SMEIL.funName = i } )

addFunInport :: SMEIL.Ident -> Codegen ()
addFunInport i = modifyCFun (\s -> s { SMEIL.funInports = SMEIL.funInports s `mappend` pure i })

addFunOutport :: SMEIL.Ident -> Codegen ()
addFunOutport i = modifyCFun (\s -> s { SMEIL.funOutports = SMEIL.funOutports s `mappend` pure i})

addFunLocal :: SMEIL.Decl -> Codegen ()
addFunLocal d = modifyCFun (\s -> s { SMEIL.locals = SMEIL.locals s `mappend` pure d })

addFunStmt :: SMEIL.Stmt -> Codegen ()
addFunStmt d = modifyCFun (\s -> s { SMEIL.funBody = SMEIL.funBody s `mappend` pure d })

-- |Execute action x in a new context and merge context into CodegenState
withNewBlockContext :: Codegen () -> Codegen ()
                    -> Codegen ()
withNewBlockContext f merger = do
  curState <- isNothing <$> gets currentBlock
  when curState (setFunInterpState newFunInterpState
                  >> f
                  >> merger)

--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

stringifyList :: Expr SrcSpan -> Codegen [String]
stringifyList (PListEls els) = mapM el els
  where
    el :: Expr SrcSpan -> Codegen String
    el (PString1 s) = return $ unquote s
    el (VarIdent i) = return i
    el _ = throwError "Only lists consisting entirely of variable names, strings or numbers are supported"
stringifyList _ = throwError "Not a list"

stringifyArgList :: [Argument SrcSpan] -> Codegen [String]
stringifyArgList els = do
  (list, annots) <- mapAndUnzipM unArgExpr els
  stringifyList List { list_exprs = list
                     , expr_annot = foldr combineSrcSpans SpanEmpty annots }
  where
    unArgExpr :: Argument SrcSpan -> Codegen (Expr SrcSpan, SrcSpan)
    unArgExpr ArgExpr { arg_expr = e, arg_annot = s } = return (e, s)
    unArgExpr _ = throwError "Only simple arguments are currently supported"

stringifyParamList :: [Parameter SrcSpan] -> Codegen [String]
stringifyParamList = mapM unParam
  where
    unParam :: Parameter SrcSpan -> Codegen String
    unParam Param { param_name = PIdent i } = return i
    unParam par =  throwError ("Unsupported parameter" ++ show par)

--------------------------------------------------------------------------------
-- Simple maps
--------------------------------------------------------------------------------

mapOp :: Op SrcSpan -> Codegen SMEIL.BinOps
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

mapUnOp :: Op SrcSpan -> Codegen SMEIL.UnOps
mapUnOp Not {} = return SMEIL.NotOp
mapUnOp _ = throwError "Unsupported unary operator"

mapAssignOp :: AssignOp SrcSpan -> Codegen (Op SrcSpan)
mapAssignOp PlusAssign {assignOp_annot = an} = return Plus {op_annot = an}
mapAssignOp MinusAssign {assignOp_annot = an} = return Minus {op_annot = an}
mapAssignOp MultAssign {assignOp_annot = an} = return Multiply {op_annot = an}
mapAssignOp LeftShiftAssign {assignOp_annot = an} = return ShiftLeft {op_annot = an}
mapAssignOp RightShiftAssign {assignOp_annot = an} = return ShiftRight {op_annot = an}
-- FIXME: Print nicer error message including annotation
mapAssignOp _ = throwError "Operator not supported"

--------------------------------------------------------------------------------
-- Interpretation definitions
--------------------------------------------------------------------------------

classifyClass :: [Argument SrcSpan] -> Maybe BaseClass
classifyClass [ArgExpr {arg_expr = VarIdent arg}]
  | arg == "External" = Just External
  | arg == "Function" = Just Function
  | arg == "Network" = Just Network
  | otherwise = Nothing
classifyClass _ = Nothing

-- |Generates code for certain toplevel classes
genStatement :: Py.Statement SrcSpan -> Codegen ()
genStatement cls@Class { class_name = Ident { ident_string = name }
                       , class_args = args
                       , class_body = body
                       } = do
  nextName <- getName
  tell [nextName]
  case classifyClass args of
    Just Function -> withNewBlockContext (mapBlockStms (genFunction name) cls) mergeState
    Just External -> genExternal name body
    Just Network -> mapBlockStms (genNetwork name) cls
    Nothing -> do
      -- FIXME Log that a class was ignored somewhere (and possibly the reason)
      tell ["Ignoring class " ++ name ++ " due to unsupported class inheritance. i.e. we have no fucking idea what the fuck it's doing (yet)"]
      return ()
  where
    mergeState :: Codegen ()
    mergeState = return () -- clearFunInterpState

genStatement _ = tell ["Ignoring unsupported top-level statement"]

genFunction :: String -> Statement SrcSpan -> Codegen ()
genFunction name s
  | funName "setup" s = setFunName name >> setupParams s >> mapBlockStms genSetup s
  | funName "run" s = mapBlockStms genRun s
  | otherwise = tell ["Ignoring unknown statement in function"]
  where

    setupParams :: Statement SrcSpan -> Codegen ()
    setupParams Fun {fun_args = args} = stringifyParamList args >>= addParamsIS
    setupParams _ = throwError "Got passed something that wasn't a function"

    genSetup :: Statement SrcSpan -> Codegen ()
    genSetup (PSelfFunCall "map_ins" (_:args) ) = do
      argList <- stringifyArgList args
      addInbusIS argList
    genSetup (PSelfFunCall "map_outs" (_:args) ) = do
      argList <- stringifyArgList args
      addOutbusIS argList
    genSetup (PSelfAssign dest (VarIdent symb)) = do
      param <- isParam symb
        -- TODO: Lookup variable binding in either list of bound variables or
        -- parameters
      if param then
        void $ addBindingIS dest $ Free symb
        else
        -- TODO: Do something useful here
        void $ addBindingIS dest $ Free $ symb ++ "_bound"
    genSetup (PSelfAssign dest (PIntLit i )) =
      addBindingIS dest $ Bound $ SMEIL.SMEInt i
    genSetup _ = tell ["Ignoring statement in setup function"]

    genRun :: Statement SrcSpan -> Codegen ()
    genRun a = genStm a >>= addFunStmt

genExpr :: Expr SrcSpan -> Codegen SMEIL.Expr
genExpr Int { int_value = n } = return $ SMEIL.Num $ SMEIL.SMEInt n
-- TODO: Check if variable is defined and check if value is a bus or variable
genExpr Subscript { subscriptee = PSelfDot v
                  , subscript_expr = PString1 s} = return $ SMEIL.Var $ v ++ "." ++ unquote s
genExpr BinaryOp { operator = op
                 , left_op_arg = l
                 , right_op_arg = r } =
  pure SMEIL.BinOp <*> mapOp op  <*> genExpr l <*> genExpr r
genExpr UnaryOp { operator = op
                , op_arg = arg } = pure SMEIL.UnOp <*> mapUnOp op <*> genExpr arg
genExpr (PSelfDot v) = return $ SMEIL.Var v
genExpr Paren { paren_expr = e } = SMEIL.Paren <$> genExpr e
genExpr _ = tell ["Unsupported expression"] >> return SMEIL.NopExpr

genStm :: Statement SrcSpan -> Codegen SMEIL.Stmt
--genStm (PSelfAssign ident expr) = pure (SMEIL.Assign ident) <*> genExpr expr
genStm Assign { assign_to = [to],
                assign_expr = expr } = do
  assignTo <- genExpr to
  case assignTo of
    SMEIL.Var i -> pure (SMEIL.Assign i) <*> genExpr expr
    _ -> throwError "Assign to expression not supported (not reducible to Var expression)"

genStm Conditional { cond_guards = guards
                   , cond_else = condElse
                   } = pure SMEIL.Cond <*> mapM genGuards guards <*> mapM genStm condElse
      where
        genGuards :: (Expr SrcSpan, Suite SrcSpan) -> Codegen (SMEIL.Expr, SMEIL.Stmts)
        genGuards (e, s) = pure (,) <*> genExpr e <*> mapM genStm s
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

genExternal :: String -> Suite SrcSpan -> Codegen ()
genExternal name _s = do
  tell ["Ignoring external class " ++ name]
  modify $ addClass name

catchFunDef :: Statement SrcSpan
               -> (Statement SrcSpan -> Bool)
               -> (Statement SrcSpan -> Codegen ())
               -> Codegen ()
catchFunDef stm predi act = when (predi stm) (act stm)

funName :: String -> Statement SrcSpan -> Bool
funName names Fun {fun_name = PIdent name} = name == names
funName _ _ = False

isBusDef :: Statement SrcSpan -> Bool
isBusDef Assign {assign_expr = Call {call_fun = VarIdent i}} = i == "Bus"
isBusDef _ = False

-- TODO: Only remove quotation marks at start or end of string
unquote :: String -> String
unquote = filter (`notElem` "\"'")

mapBlockStms :: (Statement SrcSpan -> Codegen ()) -> Statement SrcSpan -> Codegen ()
mapBlockStms f Fun {fun_body = body} = mapM_ f body
mapBlockStms f Class {class_body = body} = mapM_ f body
mapBlockStms f s = do
  tell ["WARNING: called mapBlockStms on statements other fhan Fun or Class"]
  f s


genNetwork :: String -> Statement SrcSpan -> Codegen ()
genNetwork _name stm = when (funName "wire" stm) (mapBlockStms networkDef stm)
  where

    mapType :: String -> Codegen SMEIL.DType
    mapType "int" = return SMEIL.IntType
    mapType "float" = return SMEIL.FloatType
    mapType s = throwError $ "Unsupported bus type " ++ s

    networkDef :: Statement SrcSpan -> Codegen ()
    networkDef (PClassAssign varName (VarIdent "Bus")
                [ ArgExpr { arg_expr = PString1 bname }
                , ArgExpr { arg_expr = parlist }
                , ArgExpr { arg_expr = VarIdent btype }
                ]
               ) = do
      smetype <- mapType btype
      ports <- stringifyList parlist
      addBus varName SMEIL.Bus { SMEIL.busName = unquote bname
                               , SMEIL.busDtype = smetype
                               , SMEIL.busPorts = ports
                               }
    networkDef (PClassAssign varName (VarIdent instof)
                ( ArgExpr { arg_expr = PString1 iname }
                : ArgExpr { arg_expr = inbusses }
                : ArgExpr { arg_expr = outbusses }
                -- FIXME: Handle arbitrary parameters
                : _rest )) = do
      inbuss <- stringifyList inbusses
      outbuss <- stringifyList outbusses
      addInstance varName SMEIL.Instance { SMEIL.instName = unquote iname
                                         , SMEIL.instFun = instof
                                         , SMEIL.inBusses = inbuss
                                         , SMEIL.outBusses = outbuss
                                         }
    networkDef Fun {fun_name = _n}  = tell ["there shouldn't be a function here "]
    networkDef _ = tell ["Ignoring unknown statement in busdef"]

genModule :: Module SrcSpan -> Codegen ()
genModule (Module m) = mapM_ genStatement m

pyToSMEIL :: ModuleSpan -> Either GenError (CodegenState, Log)
pyToSMEIL pymod = runCodegen newState (genModule pymod)

-- TODO: Use error message: Reference to free variable .. somewhere

{- Idea: values of setup function parameter are replaced by functions which  -}
{- functions update their corresponding free variable when executed. Can be  -}
{- though of as triggers or deferred actions that are completed when the     -}
{- information needed to complete them becomes available. In some ways, it's -}
{- kinda like a monad                                                        -}
