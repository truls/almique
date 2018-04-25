{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.PySMEIL.Instances ( Variable (..) ) where

import           Language.PySMEIL.AST

-- TODO: Is there a better way to do this?
instance Ord Variable where
  a `compare` b = s a `compare` s b
    where
      s (ParamVar _ i) = "param_" ++ i
      s (ConstVar _ i) = "const_" ++ i
      s (BusVar _ i j) = "bus_" ++ i ++ "_" ++ j
      s (NamedVar _ i) = "name_" ++ i
