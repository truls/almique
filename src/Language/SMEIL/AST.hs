-- Abstract syntax tree of the SME intermediate language used for representing
-- SME networks

module Language.SMEIL.AST
       ( BinOps(..)
       , UnOps(..)
       , Ident
       , Expr(..)
       , Stmt(..)
       , Stmts(..)
       , Function(..)
       , Network(..)
       , SMEdtype(..)
       , Map(..)
       , Instance(..)
       , Bus(..)
       , SMENum(..)
       , Decl(..)
       , DType(..)
       , FunType(..)
       , Variable(..)
       , PrimVal(..)
       , SMEBool(..)
       ) where

type Ident = String
data SMEdtype = SMEdtype SMENum deriving (Eq, Show)

-- TODO: Maybe add a PrimVal type for un-reducable values (string, numbers, etc..._

data PrimVal = Num SMENum
             | Bool SMEBool
             | EmptyVal
             deriving (Eq, Show)

data SMENum = SMEInt Integer
            | SMEFloat Double
            deriving (Eq, Show)

data SMEBool = SMETrue
             | SMEFalse
             deriving (Eq, Show)

data DType = IntType Int
           | UIntType Int
           | FloatType Int
           | BoolType
           | AnyType
            deriving (Eq, Show)

-- data PrimVal = PrimVal SMENum Integer
--                deriving (Eq, Show)

data Network = Network { netName :: Ident
                       , functions :: [Function]
                       , busses :: [Bus]
                       , instances :: [Instance]
                       }
               --, mappings :: [Map]}
             deriving (Eq, Show)

-- type Network = [Function]

data Bus = Bus { busName :: Ident
               , busDtype :: DType
               , busPorts :: [Ident]
               }
         deriving (Eq, Show)

data Map = Map { srcPort :: Ident
               , dstPort :: Ident
               }
         deriving (Eq, Show)

data Decl = Decl Variable DType (Maybe Expr)
          deriving (Eq, Show)

data FunType = Complete
             | Skeleton
             | Undecided
             deriving (Eq, Show)

data Function = Function { funName :: Ident
                         , funInports :: [(Ident, Ident)]
                         , funOutports :: [(Ident, Ident)]
                         , funParams :: [Decl]
                         , locals :: [Decl]
                         , funBody :: Stmts
                         , funType :: FunType
                         }
              deriving (Eq, Show)

data Instance = Instance { instName :: Ident
                         , instFun :: Ident
                         -- FIXME: Separate type for bus defs?
                         , inBusses :: [Ident]
                         , outBusses :: [Ident]
                         , instParams :: [(Ident, PrimVal)]
                         }
              deriving (Eq, Show)

data Stmts = Stmts [Stmt]
           deriving (Eq, Show)

-- Hack
instance Monoid Stmts where
  (Stmts a) `mappend` (Stmts b) = Stmts $ a `mappend` b
  mempty = Stmts []

data Stmt = Assign Variable Expr
          | Cond [(Expr, Stmts)] Stmts
          | NopStmt
          deriving (Eq, Show)

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
        deriving (Eq, Show)

data UnOps = NotOp
           deriving (Eq, Show)

data Variable = ConstVar DType Ident
              | BusVar DType Ident Ident
              | NamedVar DType Ident
              | ParamVar DType Ident
              deriving Show

-- | Variable _names_ should be unique independent of their kind
instance Eq Variable where
  a == b = vname a == vname b
    where
      vname (ConstVar _ n) = n
      vname (NamedVar _ n) = n
      vname (BusVar _ n m) = n ++ "_" ++ m
      vname (ParamVar _ n) = n

data Expr = BinOp { op :: BinOps
                  , left :: Expr
                  , right :: Expr
                  }
          | UnOp { unOp :: UnOps
                 , unOpVal :: Expr
                 }
          | Prim PrimVal
          | Var Variable
          | Paren Expr
          | NopExpr
          deriving (Eq, Show)
