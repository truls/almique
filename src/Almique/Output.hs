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

entity :: Ident -> PortList -> Doc
entity s d = pp Entity <+> text s <+> pp Is
  $+$ indent (pp Port <+> parens
              ( indent (d
                        $+$ text "rst: in std_logic;"
                        $+$ text "clk: in std_logic;"
                        $+$ blank)) <> semi)
  $+$ pp End <+> text s <> semi

funPortNames :: (Ident, Ident) -> Reader Network [Doc]
funPortNames (n, t) = do
  ps <- findPred busses (\bn -> t == busName bn) busPorts
  return $ map (\s -> underscores [n,s]) (fromMaybe [] ps)

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

makeVarVal :: Maybe Expr -> Doc
makeVarVal v = fromMaybe empty ((\e -> space <> pp Gets <+> pp e) <$> v)

makeVar :: Decl -> Doc
makeVar (Decl (NamedVar n) v) = pp Variable <+> text n <> colon
  <+> text "type" <> makeVarVal v <> semi
makeVar (Decl (ConstVar n) v) = pp Constant <+> text n <> colon
  <+> text "type" <> makeVarVal v <> semi

process :: Ident -> FunBody -> SensitivityList -> Reader Network Doc
process fname body sensitivity = do
  -- XXX: Why not pass function from caller?
  vars <- fromMaybe [] <$> findPred functions (\s -> fname == funName s) locals
  return (pp Process <+> parens ( empty $+$ sensitivity
                                                 )
          -- TODO: Split definitions on constants and variables
          $+$ vcat (map makeVar vars)
          $+$ pp Begin
          $+$ indent (pp If <+> text "rst = '1'" <+> pp Then
                      $+$ indent (text "-- Reset stuff goes here")
                      $+$ pp Elsif <+> pp (RisingEdge (text "clk")) <+> pp Then
                      $+$ indent body
                     )
          $+$ pp EndIf <> semi)



inst :: Instance -> Reader Network Doc
inst Instance { instName = name
              , instFun = fun
              , inBusses = inbus
              , outBusses = outbus
              , instParams = params
              } = return $ text name <> colon <+> pp Entity <+> text fun
  $+$ pp PortMap <+> parens (indent ( empty
                                      $+$ text "foo" <+> pp MapTo <+> text "bar"
                                      $+$ text "baz" <+> pp MapTo <+> text "foo"
                                      $+$ clockedMap
                                    ) <> semi)



topPorts :: Reader Network Doc
topPorts = do
  sigdefs <- asks busses >>= mapM busSigs
  return $ vcat sigdefs
    where
      busSigs :: Bus -> Reader Network Doc
      busSigs b = do
        nn <- asks netName
        let bn = busName b
        let bp = busPorts b
        return $ vcat $ map (\s -> underscores [nn, bn, s]
                                   <> colon <+> pp InOut <+> text "type"
                            ) bp

--topPortMap :: Reader Network Doc

makeTopLevel :: Reader Network Doc
makeTopLevel = do
  nname <- asks netName
  ports <- topPorts
  insts <- asks instances >>= mapM inst
  return $ entity nname ports
    $+$ architecture nname (text "signals") (vcat insts)
{-|

Toplevel: in entity, for every bus referenced by instances

-}

vhdlExt :: FilePath -> FilePath
vhdlExt = flip (++) ".vhdl"

makeFun :: Function -> Reader Network OutputFile
makeFun f = do
  let fname = funName f
  ports <- entPorts f
  procc <- process fname (pp (funBody f))(vcat [ text "clk" <> comma , text "rst"])
  return OutputFile { dir = ""
                    , file = vhdlExt fname
                    , output = header
                      $+$  entity fname ports
                      $+$ architecture fname empty procc
                    }

makeNetwork :: Reader Network OutputPlan
makeNetwork = do
  nn <- asks netName
  funs <- asks functions
  outFuns <- mapM makeFun funs
  tl <- makeTopLevel
  return $ OutputFile { dir = ""
                      , file = vhdlExt nn
                      , output = header
                        $+$ tl
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
