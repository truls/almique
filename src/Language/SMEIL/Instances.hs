{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.SMEIL.Instances ( Variable (..) ) where

import Language.SMEIL.AST

-- TODO: Is there a better way to do this?
instance Ord Variable where
  a `compare` b = s a `compare` s b
    where
      s (ConstVar i) = "const_" ++ i
      s (BusVar i j) = "bus_" ++ i ++ "_" ++ j
      s (NamedVar i) = "name_" ++ i
