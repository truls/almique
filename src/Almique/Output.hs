{-# LANGUAGE ScopedTypeVariables #-}

module Almique.Output
  ( execPlan
  , makePlan
  ) where

import Text.PrettyPrint
import Control.Monad.Reader
import Data.Maybe (fromMaybe)

import System.Directory

import Language.SMEIL
import Language.SMEIL.VHDL

type PortList = Doc
type PortMap = Doc
type SensitivityList = Doc
type SignalList = Doc
type FunBody = Doc
type ArchitectureBody = Doc

-- TODO: Split into two modules: One for doing the file handling stuff and one
-- for rendering the actual output

data OutputFile = OutputFile { dir :: FilePath
                             , file :: FilePath
                             , output :: Doc
                             }
                  deriving Show

type OutputPlan = [OutputFile]

concatPath :: OutputFile -> FilePath
concatPath OutputFile { dir = d
                      , file = f
                      } = d ++ "/" ++ f

-- FIXME: This is really bad. Ideally, we shouldn't have to do this kind of
-- cross referencing when generating VHDL from our AST. Maybe a symptom of
-- poor data structure design choices?
findPred :: forall a b. (Network -> [a])
            -> (a -> Bool)
            -> (a -> b)
            -> Reader Network (Maybe b)
findPred n p f = asks n >>= pure . locate
  where
    locate :: [a] -> Maybe b
    locate [] = Nothing
    locate (e:es)
      | p e = Just $ f e
      | otherwise = locate es

makeTopLevel :: Reader Network Doc
makeTopLevel = undefined

entity :: Ident -> PortList -> Doc
entity s d = pp Entity <+> text s <+> pp Is
  $+$ indent (pp Port <+> parens
              ( indent (d
                        $+$ text "rst: std_logic;"
                        $+$ text "clk: std_logic;")) <> semi)
  $+$ text "end" <+> text s <> semi

funPortNames :: (Ident, Ident) -> Reader Network [Doc]
funPortNames (n, t) = do
  ps <- findPred busses (\bn -> t == busName bn) busPorts
  return $ map (\s -> text (n ++ "_" ++ s)) (fromMaybe [] ps)

-- |Generates a list of input and output ports 
entPorts :: Function -> Reader Network Doc
entPorts Function { funInports = ins
                  , funOutports = outs } = vcat <$> sequence (map (ports Out) outs ++ map (ports In) ins)
  where
    ports :: VHDLKw -> (Ident, Ident) -> Reader Network Doc
    ports d ps = do
      names <- funPortNames ps
      return $ vcat $
        map (\s -> s <> colon <+> pp d <+> text "type" <> semi) names

architecture :: Ident -> SignalList -> Doc -> Doc
architecture s signals body = pp Architecture <+> text "RTL" <+> pp Of <+> text s <+> pp Is
  $+$ indent signals
  $+$ pp Begin
  $+$ indent body
  $+$ pp EndArchitecture <> semi

process :: FunBody -> SensitivityList -> Doc
process body sensitivity = pp Process <+> parens ( empty
                                                   $+$ sensitivity
                                                 )
  -- TODO: Variable declarations
  $+$ pp Begin
  $+$ indent (pp If <+> text "rst = '1'" <+> pp Then
              $+$ indent (text "-- Reset stuff goes here")
              $+$ pp Elsif <+> pp (RisingEdge (text "clk")) <> pp Then
              $+$ indent body
             )
  $+$ pp EndIf <> semi


inst :: Instance -> Reader Network Doc
inst Instance { instName = name
              , instFun = fun
              , inBusses = inbus
              , outBusses = outbus
              , instParams = params
              } = return $ text name <> colon <+> pp Entity <+> text fun
  $+$ pp Port <+> pp PortMap <+> parens (indent ( empty
                                                  $+$ text "foo" <+> pp MapTo <+> text "bar"
                                                  $+$ text "baz" <+> pp MapTo <+> text "foo"
                                                  $+$ clockedMap
                                                ) <> semi)



{-|

Toplevel: in entity, for every bus referenced by instances

-}

vhdlExt :: FilePath -> FilePath
vhdlExt = flip (++) ".vhdl"

makeFun :: Function -> Reader Network OutputFile
makeFun f = let
  fname = funName f
  in do
  ports <- entPorts f
  return OutputFile { dir = ""
                    , file = vhdlExt fname
                    , output = header
                      $+$  entity fname ports
                      $+$ architecture fname empty (process (pp (funBody f))
                                                    (vcat [ text "clk" <> comma
                                                          , text "rst"]))
                    }

makeNetwork :: Reader Network OutputPlan
makeNetwork = do
  nn <- asks netName
  funs <- asks functions
  outFuns <- mapM makeFun funs
  return $ OutputFile { dir = ""
                      , file = vhdlExt nn
                      , output = text "--This is the toplevel file"
                      } : outFuns

makePlan :: Network -> OutputPlan
makePlan = runReader makeNetwork

execPlan :: OutputPlan -> IO ()
execPlan = mapM_ (makeOutput . spliceDir)
  where
    makeOutput :: OutputFile -> IO ()
    makeOutput outf@OutputFile { dir = d
                               , output = o
                               } = do
      createDirectoryIfMissing True d
      let path = concatPath outf
      exists <- doesFileExist path
      if exists then
        fail $ "File " ++ path ++ " already exists"
        else
        writeFile path (render o)

    spliceDir :: OutputFile -> OutputFile
    spliceDir x = x { dir = "output" }
