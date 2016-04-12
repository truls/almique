{-# LANGUAGE DeriveGeneric #-}
-- Abstract syntax tree of the SME used for representing
-- SME networks

module Language.SMEIL.AST
       ( BinOps(..)
       , UnOps(..)
       , Ident
       , Expr(..)
       , Stmt(..)
       , Stmts
       , Function(..)
       , Network(..)
       , SMEdtype(..)
       , Map(..)
       , Instance(..)
       , Bus(..)
       , SMENum(..)
       , Decl(..)
       , DType(..)
         --       , PrimVal(..)
       ) where

import Text.PrettyPrint.GenericPretty

type Ident = String
data SMEdtype = SMEdtype SMENum deriving (Generic,Eq, Show)

-- TODO: Maybe add a PrimVal type for un-reducable values (string, numbers, etc..._

data SMENum = SMEInt Integer
            | SMEFloat Float
              deriving (Generic,Eq, Show)

data DType = IntType
            | FloatType
              deriving (Eq, Show)

-- data PrimVal = PrimVal SMENum Integer
--                deriving (Eq, Show)

data Network = Network { functions :: [Function]
                       , busses :: [Bus]
                       , instances :: [Instance]
                       }
               --, mappings :: [Map]}
             deriving (Generic,Eq, Show)

-- type Network = [Function]

data Bus = Bus { busName :: Ident
               , busDtype :: DType
               , busPorts :: [Ident]
               }
         deriving (Generic,Eq, Show)

data Map = Map { srcPort :: Ident
               , dstPort :: Ident
               }
         deriving (Generic,Eq, Show)

data Decl = Decl Ident (Maybe SMENum)
            deriving (Eq, Show)

data Function = Function { funName :: Ident
                         , funInports :: [Ident]
                         , funOutports :: [Ident]
                         , locals :: [Decl]
                         , funBody :: [Stmt]
                         }
              deriving (Generic,Eq, Show)

data Instance = Instance { instName :: Ident
                         , instFun :: Ident
                         , inBusses :: [Ident]
                         , outBusses :: [Ident]
                         }
                deriving (Generic,Eq, Show)

type Stmts = [Stmt]
data Stmt = Assign Ident Expr
          | Cond [(Expr, Stmts)] Stmts
          | NopStmt
          deriving (Generic,Eq, Show)

data BinOps = PlusOp
            | MinusOp
            | MulOp
            | EqOp
            | NeqOp
            | DivOp
            | OrOp
            | XorOp
            | AndOp
            | SLOp
            | SROp
        deriving (Generic,Eq, Show)

data UnOps = NotOp
           deriving (Generic,Eq, Show)

data Expr = BinOp { op :: BinOps
                  , left :: Expr
                  , right :: Expr
                  }
          | UnOp { unOp :: UnOps
                 , unOpVal :: Expr
                 }
          | Num SMENum
          | Var Ident
          | Paren Expr
          | NopExpr
          deriving (Generic,Eq, Show)
