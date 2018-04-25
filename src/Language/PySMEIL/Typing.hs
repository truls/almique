module Language.PySMEIL.Typing
  ( typeOf
  , sizeOf
  , sign
  , Signedness(..))
where

import           Language.PySMEIL.AST

-- VERY simple for now. Shouldn't be hard to do things a little bit better
-- e.g.by unifying integer types of different sizes to the biggest of the two
unify :: DType -> DType -> DType
unify a b
  | a == b = a
  | a == AnyType && b /= AnyType = b
  | a /= AnyType && b == AnyType = a
  | otherwise = AnyType

data Signedness = IsSigned | IsUnsigned

class TypeOf a where
  typeOf :: a -> DType

instance TypeOf Variable where
  typeOf (ParamVar t _)  = t
  typeOf (ConstVar t _)  = t
  typeOf (BusVar t _ _)  = t
  typeOf (NamedVar t _ ) = t

instance TypeOf PrimVal where
  typeOf (Num n)  = typeOf n
  typeOf (Bool b) = typeOf b
  typeOf EmptyVal = AnyType

instance TypeOf SMENum where
  typeOf (SMEInt _)   = IntType 32
  typeOf (SMEFloat _) = FloatType 32

instance TypeOf SMEBool where
  typeOf SMETrue  = BoolType
  typeOf SMEFalse = BoolType

instance TypeOf Expr where
  typeOf BinOp { left = l
               , right = r } = unify (intToAny l) (intToAny r)
    where
      intToAny (Prim (Num (SMEInt _))) = AnyType
      intToAny a                       = typeOf a
  typeOf UnOp { unOpVal = v } = typeOf v
  typeOf (Prim p) = typeOf p
  typeOf (Paren e) = typeOf e
  typeOf (Var v) = typeOf v
  typeOf NopExpr = AnyType

sizeOf :: DType -> Int
sizeOf (IntType s)  = s
sizeOf (UIntType s) = s
sizeOf _            = 0

sign :: DType -> Signedness
sign (IntType _)   = IsSigned
sign (UIntType _)  = IsUnsigned
sign BoolType      = IsUnsigned
sign (FloatType _) = IsSigned
sign AnyType       = IsSigned
