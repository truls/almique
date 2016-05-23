module Language.SMEIL.Typing ( typeOf ) where

import Language.SMEIL.AST

-- VERY simple for now. Shouldn't be hard to do things a little bit better
-- e.g.by unifying integer types of different sizes to the biggest of the two
unify :: DType -> DType -> DType
unify a b
  | a == b = a
  | otherwise = AnyType

class TypeOf a where
  typeOf :: a -> DType

instance TypeOf Variable where
  typeOf (ParamVar t _) = t
  typeOf (ConstVar t _) = t
  typeOf (BusVar t _ _) = t
  typeOf (NamedVar t _ ) = t

instance TypeOf Bus where
  typeOf Bus { busDtype = t } = t

instance TypeOf PrimVal where
  typeOf (Num n) = typeOf n
  typeOf (Bool b) = typeOf b
  typeOf EmptyVal = AnyType

instance TypeOf SMENum where
  typeOf (SMEInt _) = IntType 32
  typeOf (SMEFloat _) = FloatType 32

instance TypeOf SMEBool where
  typeOf SMETrue = BoolType
  typeOf SMEFalse = BoolType

instance TypeOf Expr where
  typeOf BinOp { left = l
               , right = r } = unify (typeOf l) (typeOf r)
  typeOf UnOp { unOpVal = v } = typeOf v
  typeOf (Prim p) = typeOf p
  typeOf (Paren e) = typeOf e
  typeOf (Var v) = typeOf v
  typeOf NopExpr = AnyType
