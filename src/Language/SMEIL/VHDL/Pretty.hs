module Language.SMEIL.VHDL.Pretty
       ( indent
       , unindent
       , blank
       , underscores
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
            | Gets
            deriving Show

data VHDLFuns = RisingEdge Doc
              | StdLogicVector Doc
              | ToUnsigned Doc Doc
              | Unsigned Doc

instance Pretty VHDLFuns where
  pp (RisingEdge d) = text "rising_edge" <> parens d
  pp (StdLogicVector d) = text "std_logic_vector" <> parens d
  pp (ToUnsigned d1 d2) = text "to_unsigned" <> parens (d1 <> comma <> space <> d2)
  pp (Unsigned d) = text "unsigned" <> parens d

class Pretty a where
  pp :: a -> Doc

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

instance Pretty Variable where
  pp (ConstVar v) = text v
  pp (NamedVar v) = text v
  pp (BusVar i v) = text $ i ++ "_" ++ v

instance Pretty Stmts where
  pp (Stmts s) = vcat $ map pp s

instance Pretty Stmt where
  pp (Assign v e) = case v of
    n@(NamedVar _) -> pp n <+> text ":=" <+> pp e
    n@(BusVar _ _) -> pp n <+> text "<=" <+> pp e
    (ConstVar _) -> text "-- Assignment to constvar attempted"
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

instance Pretty Expr where
  pp BinOp { op = p
           , left = l
           , right = r
           } = pp l <+> pp p <+> pp r
  pp UnOp { unOp = u
          , unOpVal = v
          } = pp u <+> pp v
  pp (Prim p) = pp p
  pp (Var v) = pp v
  pp (Paren e) = parens $ pp e
  pp NopExpr = empty

instance Pretty BinOps where
  pp PlusOp = text "+"
  pp MinusOp = text "-"
  pp MulOp = text "*"
  pp EqOp = text "="
  pp NeqOp = text "/="
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
  pp MapTo = text "=>"
  pp Gets = text ":="
  pp r = text $ map toLower (show r)
