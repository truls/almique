module Language.SMEIL.VHDL.Snippets
       ( header
       , clockedSignals
       , clockedMap
       )
       where

import Text.PrettyPrint


header :: Doc
header = text $ unlines [
  "library ieee;"
  ,"use ieee.std_logic_1164.all;"
  ,"use ieee.std_logic_unsigned.all;"
  ,"use ieee.numeric_std.all;"
  ]

clockedSignals :: Doc
clockedSignals = text $ unlines [
  "-- Reset signal"
  ,"rst: std_logic;"
  ,""
  ,"-- Clock signal"
  ,"clk: std_logic;"
  ]

clockedMap :: Doc
clockedMap = empty $+$ text "rst => rst," $+$ text "clk => clk"
