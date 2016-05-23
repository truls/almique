{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.SMEIL.Instances ( Variable (..) ) where

import Language.SMEIL.AST

-- TODO: Is there a better way to do this?
instance Ord Variable where
  a `compare` b = s a `compare` s b
    where
      s (ConstVar _ i) = "const_" ++ i
      s (BusVar _ i j) = "bus_" ++ i ++ "_" ++ j
      s (NamedVar _ i) = "name_" ++ i

