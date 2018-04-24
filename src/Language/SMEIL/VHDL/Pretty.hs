{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.SMEIL.VHDL.Pretty
       ( indent
       , unindent
       , blank
       , underscores
       , commas
       , primCast
       , primDefaultVal
       , assignCast
       , varCast
       , typeName
       , Pretty
       , pp
       , pp'
       , VHDLKw(..)
       , VHDLFuns(..)
       , VHDLAttribs(..)
       )
       where

import           Prelude          hiding ((<>))

import           Data.Char        (toLower)
import           Text.PrettyPrint

import           Language.SMEIL

import           Debug.Trace

indentWidth :: Int
indentWidth = 2

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

data VHDLKw = Architecture
            | Begin
            | BusGets
            | Body
            | Constant
            | Downto
            | Else
            | Elsif
            | End
            | EndArchitecture
            | EndIf
            | EndLoop
            | EndProcess
            | Entity
            | For
            | Fun
            | Generic
            | GenericMap
            | Gets
            | If
            | In
            | InOut
            | Integer
            | Is
            | Loop
            | MapTo
            | Not
            | Of
            | Out
            | Package
            | Port
            | PortMap
            | Process
            | Pure
            | PureFunction
            | Report
            | Return
            | ShiftLeft
            | ShiftRight
            | Signal
            | StdLogic
            | StdLogicVec Int Int
            | Subtype
            | Then
            | Until
            | Variable
            | Wait
            | WaitFor
            | While
            | Work
            deriving Show

data VHDLFuns = RisingEdge Doc
              | StdLogicVector Doc
              | ToUnsigned Doc Doc
              | ToSigned Doc Doc
              | Unsigned Doc
              | Signed Doc
              | Resize Doc Int
              | ReadLine Doc Doc
              | FileOpen Doc Doc Doc
              | EndFile Doc
              | FileClose Doc

instance Pretty VHDLFuns where
  pp (RisingEdge d) = text "rising_edge" <> parens d
  pp (StdLogicVector d) = text "std_logic_vector" <> parens d
  pp (ToUnsigned d1 d2) = text "to_unsigned" <> parens (d1 <> comma <> space <> d2)
  pp (ToSigned d1 d2) = text "to_signed" <> parens (d1 <> comma <> space <> d2)
  pp (Unsigned d) = text "unsigned" <> parens d
  pp (Signed d) = text "signed" <> parens d
  pp (Resize d i) = text "resize" <> parens (d <> comma <+> pp i)
  pp (ReadLine d1 d2) = pp "readline" <> parens (d1 <> comma <+> d2)
  pp (FileOpen d1 d2 d3) = pp "file_open" <> parens (d1 <> comma <+>
                                                     d2 <> comma <+>
                                                     d3 <> comma <+>
                                                     text "READ_MODE")
  pp (EndFile d) = pp "endfile" <> parens d
  pp (FileClose d) = pp "file_close" <> parens d


data VHDLAttribs = Length Doc

instance Pretty VHDLAttribs where
  pp (Length d) = d <> pp "'length"

class Pretty a where
  pp :: a -> Doc
  -- FIXME: Kind of a hack?
  pp' :: a -> DType -> Doc
  pp' e _ = pp e

instance Pretty Int where
  pp n = integer $ fromIntegral n

instance Pretty Bool where
  pp True  = pp "true"
  pp False = pp "false"

instance Pretty String where
  pp = text

primDefaultVal :: DType -> Doc
primDefaultVal t@(IntType _) = assignCast t (pp $ ToSigned (pp "0") (pp $ Length (pp t)))
primDefaultVal t@(UIntType _) =  assignCast t (pp $ ToUnsigned (pp "0") (pp $ Length (pp t)))
primDefaultVal (FloatType _)  = pp "0.0"
primDefaultVal BoolType = pp "'0'"
primDefaultVal AnyType = empty
--primDefaultVal t@(SMEInt l) -> assignCast t $ ToSigned (pp 0) (pp l)

toSignedness :: DType -> Doc -> Doc
toSignedness t@(IntType _) d  = pp $ ToSigned d (pp $ Length (pp t))
toSignedness t@(UIntType _) d = pp $ ToUnsigned d (pp $ Length (pp t))
toSignedness _ d              = d

primCast :: PrimVal -> DType -> Doc
primCast (Num i@(SMEInt _l)) t   = toSignedness t (pp i)
primCast (Num f@(SMEFloat _)) _t = pp f
primCast (Bool b) _t             = pp b
primCast o _t                    = pp o

instance Pretty PrimVal where
  pp (Num n)  = pp n
  pp (Bool n) = pp n
  pp EmptyVal = text "This shouldn't be here"

instance Pretty SMENum where
  pp (SMEInt i)   = integer i
  pp (SMEFloat f) = double f

instance Pretty SMEBool where
  pp SMETrue  = text "'1'"
  pp SMEFalse = text "'0'"

typeName :: DType -> Doc
typeName (IntType l)  = text "i" <> pp l
typeName (UIntType l) = text "u" <> pp l
typeName t            = pp t

instance Pretty DType where
  pp (IntType l)   = text "i" <> pp l <> text "_t"
  pp (UIntType l)  = text "u" <> pp l <> text "_t"
  pp (FloatType l) = text "f" <> pp l <> text "_t"
  pp BoolType      = text "bool_t"
  pp AnyType       = text "ANY_TYPE"

instance Pretty Variable where
  pp (ParamVar _t v) = text v
  pp (ConstVar _t v) = text v
  pp (NamedVar _t v) = text v
  pp (BusVar _t i v) = text $ i ++ "_" ++ v

instance Pretty Stmts where
  pp (Stmts s) = vcat $ map pp s

assignCast :: DType -> Doc -> Doc
assignCast (IntType _) d    = pp $ StdLogicVector d
assignCast (UIntType _) d   = pp $ StdLogicVector d
assignCast (FloatType _l) d = pp "-- Floats not supported" <+> d
assignCast BoolType d       =  d
assignCast AnyType d        = pp "ANY " <> d

instance Pretty Stmt where
  pp (Assign v e) = case v of
    n@(NamedVar t _) -> pp n <+> pp Gets <+> assignCast t (pp' e t) <> semi
    n@(BusVar t _ _) -> pp n <+> pp BusGets <+> assignCast t (pp' e t) <> semi
    (ConstVar _ _) -> text "-- Assignment to constvar attempted"
    (ParamVar _ _) -> text "-- Assignment to generic value"
  pp (Cond ((e, s):cs) es) = pp If <+> pp' e (typeOf e) <+> pp Then $+$
    indent (pp s) $+$
    vcat (map (\(e', s') -> (pp Elsif <+> pp' e' (typeOf e') <+> pp Then) $+$
                            indent (pp s')) cs) $+$ maybeElse es $+$ pp EndIf <> semi
    where
      maybeElse s'@(Stmts ss)
        | not (null ss) = pp Else $+$ indent (pp s')
        | otherwise = empty

  pp (Cond [] _) = empty
  pp NopStmt = text "--"

varCast :: DType -> Doc -> Doc
varCast (IntType _) d   = pp $ Signed d
varCast (UIntType _) d  = pp $ Unsigned d
varCast (FloatType _) d = pp "-- Floats not supported" <+> d
varCast BoolType d      = d
varCast AnyType d       = pp "ANY " <+> d

-- Params that must evaluate a primitive and constant VHDL value should be
-- printed without type annotations
paramExpr :: Expr -> Doc
paramExpr BinOp { op = o
                , left = l
                , right = r
                } = pp l <+> pp o <+> pp r
paramExpr UnOp { unOp = u
               , unOpVal = v
               } = pp u <+> pp v
paramExpr (Prim p) = pp p
paramExpr (Var v) = pp v
paramExpr (Paren e) = pp e
paramExpr NopExpr = empty

instance Pretty Expr where
  pp BinOp { op = SLOp
           , left = l
           , right = r
           } = pp ShiftLeft <> parens (pp l <> comma <+> paramExpr r)
  pp BinOp { op = SROp
           , left = l
           , right = r
           } = pp ShiftRight <> parens (pp l <> comma <+> paramExpr r)
  pp BinOp { op = o@MulOp
           , left = l
           , right = r
           } = pp $ Resize (pp l <+> pp o <+> pp r) 32
  pp BinOp { op = p
           , left = l
           , right = r
           } = pp l <+> pp p <+> pp r
  pp UnOp { unOp = u
          , unOpVal = v
          } = pp u <+> pp v
  pp (Prim p) = primCast p (typeOf p)
  -- FIXME: Assuming all params are integers
  pp (Var v@ParamVar{}) = pp $ ToSigned (pp v) (pp $ Length (pp $ typeOf v))
  pp (Var v) = varCast (typeOf v) $ pp v
  pp (Paren e) = parens $ pp e
  pp NopExpr = empty
  pp' BinOp { op = SLOp
             , left = l
             , right = r
             } t = pp ShiftLeft <> parens (pp' l t <> comma <+> paramExpr r)
  pp' BinOp { op = SROp
             , left = l
             , right = r
             } t = pp ShiftRight <> parens (pp' l t <> comma <+> paramExpr r)
  pp' BinOp { op = o@MulOp
           , left = l
           , right = r
           } t = pp $ Resize (pp' l t <+> pp o <+> pp' r t) $ sizeOf t
  pp' BinOp { op = p
           , left = l
           , right = r
           } t = pp' l t <+> pp p <+> pp' r t
  pp' UnOp { unOp = u
          , unOpVal = v
          } t = pp u <+> pp' v t
  pp' (Prim p) t = primCast p t
  -- FIXME: Assuming all params are integers
  pp' (Var v@ParamVar{}) t = toSignedness t (pp v)
  pp' (Var v) t = varCast (typeOf v) $ pp v
  pp' (Paren e) t = parens $ pp' e t
  pp' NopExpr t = empty


instance Pretty BinOps where
  pp PlusOp  = text "+"
  pp MinusOp = text "-"
  pp MulOp   = text "*"
  pp EqOp    = text "="
  pp NeqOp   = text "/="
  pp LeOp    = text "<"
  pp GeOp    = text ">"
  pp LeqOp   = text "<="
  pp GeqOp   = text ">="
  pp DivOp   = text "/"
  pp OrOp    = text "or"
  pp XorOp   = text "xor"
  pp AndOp   = text "and"
  pp SLOp    = text "sll"
  pp SROp    = text "srl"

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
  pp WaitFor = pp Wait <+> pp For
  pp EndLoop = pp End <+> pp Loop
  pp Fun = text "function" -- Name clash with SMEIL.AST
  pp PureFunction = pp Pure <+> pp Fun
  pp r = text $ map toLower (show r)
