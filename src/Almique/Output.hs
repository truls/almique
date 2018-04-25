{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Almique.Output
  (writeOutput
  ) where

import           Prelude               hiding ((<>))

import           Data.List             (nub)
import           Data.List.NonEmpty    (fromList)
import           Data.Loc              (noLoc)
import           Data.Maybe            (mapMaybe)
import           Data.Text             (pack)
import qualified Data.Text.IO          as TIO

import           Language.PySMEIL
import           Language.SMEIL.Pretty
import qualified Language.SMEIL.Syntax as S

mkIdent :: Ident -> S.Ident
mkIdent i = S.Ident (pack i) noLoc

mkType :: DType -> S.Typeness
mkType (IntType n)   = S.Typed $ S.Signed (Just $ fromIntegral n) noLoc
mkType (UIntType n)  = S.Typed $ S.Unsigned (Just $ fromIntegral n) noLoc
mkType (FloatType _) = S.Typed $ S.Double noLoc
mkType BoolType      = S.Typed $ S.Bool noLoc
mkType AnyType       = S.Untyped

mkBool :: SMEBool -> S.Literal
mkBool SMETrue  = S.LitTrue noLoc
mkBool SMEFalse = S.LitFalse noLoc


mkNum :: SMENum -> S.Literal
mkNum (SMEInt i)   = S.LitInt (fromIntegral i) noLoc
mkNum (SMEFloat d) = S.LitFloat d noLoc

mkPrimVal :: PrimVal -> S.Literal
mkPrimVal (Num n)  = mkNum n
mkPrimVal (Bool b) = mkBool b
mkPrimVal EmptyVal = S.LitInt 0 noLoc

mkStmts :: Stmts -> [S.Statement]
mkStmts (Stmts ss) = mapMaybe mkStmt ss

mkStmt :: Stmt -> Maybe S.Statement
mkStmt (Assign var expr) = Just $ S.Assign (fromVar var) (toExpr expr) noLoc
mkStmt (Cond cases els) =
  let (c, b, r) = splitCond cases
      e =
        case mkStmts els of
          [] -> Nothing
          s  -> Just s
  in Just $ S.If c b r e noLoc
  where
    splitCond ((e1, s1):rest) = (toExpr e1, mkStmts s1, map mkCond rest)
    splitCond []              = error "If without conditions"
    mkCond (ex, stms) = (toExpr ex, mkStmts stms)
mkStmt NopStmt = Nothing

mkUnOp :: UnOps -> S.UnOp
mkUnOp NotOp = S.NegOp noLoc

mkOp :: BinOps -> S.BinOp
mkOp o = go o noLoc
  where
    go PlusOp  = S.PlusOp
    go MinusOp = S.MinusOp
    go MulOp   = S.MulOp
    go EqOp    = S.EqOp
    go NeqOp   = S.NeqOp
    go GeOp    = S.GtOp
    go LeOp    = S.LtOp
    go GeqOp   = S.GeqOp
    go LeqOp   = S.LeqOp
    go DivOp   = S.DivOp
    go OrOp    = S.OrOp
    go XorOp   = S.XorOp
    go AndOp   = S.AndOp
    go SLOp    = S.SllOp
    go SROp    = S.SrlOp

mkNamePart :: S.Ident -> S.NamePart
mkNamePart i = S.IdentName i noLoc

class FromVar a where
  fromVar :: Variable -> a

instance FromVar S.Expr where
  fromVar (ConstVar ty i) = S.PrimName (mkType ty) (mkName (mkIdent i)) noLoc
  fromVar (BusVar ty i1 i2) =
    S.PrimName (mkType ty) (mkName2 (mkIdent i1) (mkIdent i2)) noLoc
  fromVar (NamedVar ty i) = S.PrimName (mkType ty) (mkName (mkIdent i)) noLoc
  fromVar (ParamVar ty i) = S.PrimName (mkType ty) (mkName (mkIdent i)) noLoc

instance FromVar S.Name where
  fromVar (ConstVar _ i)   = mkName (mkIdent i)
  fromVar (BusVar _ i1 i2) = mkName2 (mkIdent i1) (mkIdent i2)
  fromVar (NamedVar _ i)   = mkName (mkIdent i)
  fromVar (ParamVar _ i)   = mkName (mkIdent i)


mkName :: S.Ident -> S.Name
mkName i = S.Name (fromList [mkNamePart i]) noLoc

mkName2 :: S.Ident -> S.Ident -> S.Name
mkName2 i i2 = S.Name (fromList (map mkNamePart [i, i2])) noLoc

class ToExpr a where
  toExpr :: a -> S.Expr

instance ToExpr Expr where
  toExpr BinOp {..} = S.Binary S.Untyped (mkOp op) (toExpr left) (toExpr right) noLoc
  toExpr UnOp {..} = S.Unary S.Untyped (mkUnOp unOp) (toExpr unOpVal) noLoc
  toExpr (Var v)    = fromVar v
  toExpr (Paren e) = toExpr e
  toExpr (Prim p) = toExpr p
  toExpr NopExpr    = S.PrimLit S.Untyped (S.LitInt 0 noLoc) noLoc

instance ToExpr PrimVal where
  toExpr v = S.PrimLit S.Untyped (mkPrimVal v) noLoc

instance ToExpr String where
  toExpr i = S.PrimName S.Untyped (mkName $ mkIdent i) noLoc

mkDecl :: Decl -> S.Declaration
mkDecl (Decl (ConstVar _ i) t (Just e)) =
  S.ConstDecl $ S.Constant (mkIdent i) (mkType t) (toExpr e) noLoc
mkDecl (Decl (NamedVar _ i) t e) =
  S.VarDecl $ S.Variable (mkIdent i) (mkType t) (toExpr <$> e) Nothing noLoc
mkDecl _ = error "This decl may not occur here"

mkConstPar :: Decl -> S.Param
mkConstPar (Decl (ParamVar _ i) _ _) =
  S.Param Nothing (S.Const noLoc) (mkIdent i) noLoc
mkConstPar (Decl (NamedVar _ i) _ _) =
  S.Param Nothing (S.Const noLoc) (mkIdent i) noLoc
mkConstPar (Decl (ConstVar _ i) _ _) =
  S.Param Nothing (S.Const noLoc) (mkIdent i) noLoc
mkConstPar _ = error "Not a valid ConstPar"

mkProcs :: Network -> [S.UnitElement]
mkProcs Network {..} = map (S.UnitProc . go) functions
  where
    go Function {..} =
      S.Process
        (mkIdent funName)
        (mkInPorts ++ mkOutPorts ++ mkConstPorts)
        (map mkDecl locals)
        (mkStmts funBody)
        True
        noLoc
      where
        mkInPorts = mkPorts funInports (S.In noLoc)
        mkOutPorts = mkPorts funInports (S.Out noLoc)
        mkPorts ps dir =
          map (\x -> S.Param Nothing dir (mkIdent x) noLoc) (nub (map fst ps))
        mkConstPorts = map mkConstPar funParams

mkBus :: Bus -> S.Bus
mkBus Bus {..} = S.Bus True False (mkIdent busName) (map mkSig busPorts) noLoc
  where
    mkSig (i, t) = S.BusSignal (mkIdent i) (mkType t) Nothing Nothing noLoc

mkInst :: Instance -> S.Instance
mkInst Instance {..} =
  S.Instance
    (Just (mkIdent instName))
    Nothing
    (mkName $ mkIdent instFun)
    ((map (\i -> (Nothing, toExpr i)) (inBusses ++ outBusses)) ++
     (map (\(i, e) -> (Just (mkIdent i), toExpr e)) instParams))
    noLoc

mkNet :: Network -> S.UnitElement
mkNet Network {..} =
  S.UnitNet $
  S.Network
    (mkIdent netName)
    []
    (map (S.NetBus . mkBus) busses ++ map (S.NetInst . mkInst) instances)
    noLoc

mkProg :: Network -> S.DesignFile
mkProg n = S.DesignFile [S.DesignUnit [] (mkProcs n ++ [mkNet n]) noLoc] noLoc

writeOutput :: Network -> IO ()
writeOutput n =
  let fname = netName n ++ ".sme"
  in TIO.writeFile fname (pprr (mkProg n))
