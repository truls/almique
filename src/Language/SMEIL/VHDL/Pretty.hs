{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Language.SMEIL.VHDL.Pretty
       ( indent
       , unindent
       , blank
       , underscores
       , commas
       , Pretty
       , pp
       , VHDLKw(..)
       , VHDLFuns(..)
       )
       where

import Data.Char (toLower)
import Text.PrettyPrint

import Language.SMEIL

indentWidth :: Int
indentWidth = 3

indent :: Doc -> Doc
indent = nest indentWidth

unindent :: Doc -> Doc
unindent = nest (-indentWidth)

-- Used in combination with vcat to insert a new
blank :: Doc
blank = text ""

underscores :: [Ident] -> Doc
underscores is = hcat $ punctuate (text "_") (map text is)

commas :: [Doc] -> [Doc]
commas = punctuate comma

data VHDLKw = Entity
            | Architecture
            | Begin
            | End
            | Of
            | Is
            | Process
            | If
            | Elsif
            | Then
            | Else
            | Variable
            | Constant
            | Port
            | EndIf
            | MapTo
            | PortMap
            | In
            | Out
            | InOut
            | EndArchitecture
            | EndProcess
            | Gets
            | BusGets
            | ShiftLeft
            | ShiftRight
            | Package
            | Subtype
            | Downto
            | StdLogicVec Int Int
            | StdLogic
            | Work
            | Integer
            | Generic
            | GenericMap
            deriving Show

data VHDLFuns = RisingEdge Doc
              | StdLogicVector Doc
              | ToUnsigned Doc Doc
              | ToSigned Doc Doc
              | Unsigned Doc
              | Signed Doc

instance Pretty VHDLFuns where
  pp (RisingEdge d) = text "rising_edge" <> parens d
  pp (StdLogicVector d) = text "std_logic_vector" <> parens (d <> comma <+> pp l)
  pp (ToUnsigned d1 d2) = text "to_unsigned" <> parens (d1 <> comma <> space <> d2)
  pp (ToSigned d1 d2) = text "to_signed" <> parens (d1 <> comma <> space <> d2)
  pp (Unsigned d) = text "unsigned" <> parens d
  pp (Signed d) = text "signed" <> parens d

class Pretty a where
  pp :: a -> Doc

instance Pretty Int where
  pp n = integer $ fromIntegral n

instance Pretty String where
  pp = text

primCast :: PrimVal -> Doc
primCast (Num i@(SMEInt _)) = pp $ ToSigned (pp i) (integer 32)
primCast (Num f@(SMEFloat _)) = pp f
primCast o = pp o

instance Pretty PrimVal where
  pp (Num n) = pp n
  pp (Bool n) = pp n
  pp EmptyVal = text "This shouldn't be here"

instance Pretty SMENum where
  pp (SMEInt i) = integer i
  pp (SMEFloat f) = double f

instance Pretty SMEBool where
  pp SMETrue = text "true"
  pp SMEFalse = text "false"

instance Pretty DType where
  pp (IntType l) = text "i" <> pp l <> text "_t"
  pp (UIntType l) = text "u" <> pp l <> text "_t"
  pp (FloatType l) = text "f" <> pp l <> text "_t"
  pp BoolType = text "bool_t"
  pp AnyType = text "ANY_TYPE"

instance Pretty Variable where
  pp (ParamVar t v) = text v
  pp (ConstVar t v) = text v
  pp (NamedVar t v) = text v
  pp (BusVar t i v) = text $ i ++ "_" ++ v

instance Pretty Stmts where
  pp (Stmts s) = vcat $ map pp s

assignCast :: DType -> Doc -> Doc
assignCast (IntType _) d = pp $ StdLogicVector d
assignCast (UIntType _) d = pp $ StdLogicVector d
assignCast (FloatType l) d = pp "-- Floats not supported" <+> d
assignCast BoolType d = pp "-- Bools not supported" <+> d
assignCast AnyType d = d

instance Pretty Stmt where
  pp (Assign v e) = case v of
    n@(NamedVar t _) -> pp n <+> pp Gets <+> assignCast t (pp e)
    n@(BusVar t _ _) -> pp n <+> pp BusGets <+> assogmCast t (pp e_
    (ConstVar _ _) -> text "-- Assignment to constvar attempted"
    (ParamVar _ _) -> text "-- Assignment to generic value"
  pp (Cond ((e, s):cs) es) = pp If <+> pp e <+> pp Then $+$
    indent (pp s) $+$
    vcat (map (\(e', s') -> (pp Elsif <+> pp e' <+> pp Then) $+$
                            indent (pp s')) cs) $+$ maybeElse es $+$ pp EndIf <> semi
    where
      maybeElse s'@(Stmts ss)
        | not (null ss) = pp Else $+$ indent (pp s')
        | otherwise = empty

  pp (Cond [] _) = empty
  pp NopStmt = text ";"

varCast :: DType -> Doc -> Doc
varCast (IntType _) d = pp $ Signed d
varCast (UIntType _) d = pp $ Unsigned d
varCast (FloatType _) d = pp "-- Floats not supported" <+> d
varCast BoolType d = pp "-- Bools not supported" <+> d
varCast AnyType d = d

instance Pretty Expr where
  pp BinOp { op = SLOp
           , left = l
           , right = r
           } = pp ShiftLeft <> parens (pp l <> comma <+> pp r)
  pp BinOp { op = SROp
           , left = l
           , right = r
           } = pp ShiftRight <> parens (pp l <> comma <+> pp r)
  pp BinOp { op = p
           , left = l
           , right = r
           } = pp l <+> pp p <+> pp r
  pp UnOp { unOp = u
          , unOpVal = v
          } = pp u <+> pp v
  pp (Prim p) = primCast p
  pp (Var v@ParamVar{}) = pp v
  pp (Var v) = varCast (typeOf v) $ pp v
  pp (Paren e) = parens $ pp e
  pp NopExpr = empty

instance Pretty BinOps where
  pp PlusOp = text "+"
  pp MinusOp = text "-"
  pp MulOp = text "*"
  pp EqOp = text "="
  pp NeqOp = text "/="
  pp LeOp = text "<"
  pp GeOp = text ">"
  pp LeqOp = text "<="
  pp GeqOp = text ">="
  pp DivOp = text "/"
  pp OrOp = text "or"
  pp XorOp = text "xor"
  pp AndOp = text "and"
  pp SLOp = text "sll"
  pp SROp = text "srl"

instance Pretty UnOps where
  pp NotOp = text "not"

instance Pretty VHDLKw where
  pp PortMap = pp Port <+> text "map"
  pp EndIf = pp End <+> pp If
  pp EndArchitecture = pp End <+> pp Architecture
  pp EndProcess = pp End <+> pp Process
  pp MapTo = text "=>"
  pp Gets = text ":="
  pp BusGets = text "<="
  pp ShiftLeft = text "shift_left"
  pp ShiftRight = text "shift_right"
  pp (StdLogicVec i j) = text "std_logic_vector" <> parens (pp i <+> pp Downto <+> pp j)
  pp StdLogic = text "std_logic"
  pp GenericMap = pp Generic <+> text "map"
  pp r = text $ map toLower (show r)
