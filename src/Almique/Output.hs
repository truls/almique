{-# LANGUAGE ScopedTypeVariables #-}

module Almique.Output
  ( execPlan
  , makePlan
  ) where

import Text.PrettyPrint
import Control.Monad.Reader
import Data.Maybe (fromMaybe, isNothing)
import Data.List (sort)

import System.Directory

import Language.SMEIL
import Language.SMEIL.VHDL

import Debug.Trace

type PortList = Doc
type PortMap = Doc
type SensitivityList = Doc
type SignalList = Doc
type FunBody = Doc
type ArchitectureBody = Doc
type GenericDefs = Doc

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

-- ppIf :: Bool -> Doc -> Doc
-- ppIf c d = if c then d else empty

labelBlock :: Ident -> Doc -> Doc
labelBlock i d = pp i <> colon <+> d

entity :: Ident -> PortList -> GenericDefs -> Doc
entity s d g = pp Entity <+> text s <+> pp Is
  $+$ indent ((if isEmpty g then empty else pp Generic <+> parens g <> semi)
              $+$ (pp Port <+> parens
                   ( indent (
                       d
                       $+$ text "rst: in std_logic;"
                       $+$ text "clk: in std_logic"
                       $+$ blank)) <> semi))
  $+$ pp End <+> text s <> semi

funPortNames :: (Ident, Ident) -> Reader Network [Doc]
funPortNames (n, t) = do
  -- FIXME: This simply returns nothing if the bus referred to
  -- 1) Do static checking in Binder to make sure this cannot fail
  ps <- findPred busses (\bn -> t == busName bn) (map fst . busPorts)
  return $ map (\s -> underscores [n,s]) (fromMaybe [] ps)

funTypeNames :: (Ident, Ident) -> Reader Network [DType]
funTypeNames (_n, t) = fromMaybe [] <$> findPred busses (\bn -> t == busName bn) (map snd . busPorts)

-- |Generates a list of input and output ports
entPorts :: Function -> Reader Network Doc
entPorts Function { funInports = ins
                  , funOutports = outs } = vcat <$> sequence (map (ports Out) outs
                                                              ++ map (ports In) ins)
  where
    ports :: VHDLKw -> (Ident, Ident) -> Reader Network Doc
    ports d ps = do
      portList <- zip <$> funPortNames ps <*> funTypeNames ps
      return $ vcat $
        map (\(s, t) -> s <> colon <+> pp d <+> pp t <> semi) portList

-- TODO: Support other types than integer here
entGenerics :: Function -> Doc
entGenerics Function { funParams = p } =
  vcat $ punctuate semi $ map (\v -> pp v <> colon <+> pp Integer) (getVars p)
  where
    getVars vs = [ v | (Decl v _ _) <- vs ]

architecture :: Ident -> SignalList -> Doc -> Doc
architecture s signals body = pp Architecture <+> text "RTL" <+> pp Of <+> text s <+> pp Is
  $+$ indent signals
  $+$ pp Begin
  $+$ indent body
  $+$ pp EndArchitecture <> semi

makeVarVal :: Maybe Expr -> Doc
makeVarVal v = fromMaybe empty ((\e -> space <> pp Gets <+> assignCast (typeOf e) (pp e)) <$> v)

makeVar :: Decl -> Doc
makeVar (Decl (NamedVar _ty n) t v) = pp Variable <+> text n <> colon
  <+> pp t  <> makeVarVal v <> semi
makeVar (Decl (ConstVar _ty n) t v) = pp Constant <+> text n <> colon
  <+> pp t <> makeVarVal v <> semi
makeVar _ = empty

funSignalReset :: (Ident, Ident) -> Reader Network Doc
funSignalReset p = do
  ports <- zip <$> funPortNames p <*> funTypeNames p
  -- FIXME: Maybe its better to generate SMEIL here and pretty print it?
  return $ vcat $ map (\(a, t) -> a <+> pp BusGets <+> primDefaultVal t <> semi) ports

funVarReset :: Decl -> Doc
funVarReset (Decl v@(NamedVar ty _) _ _) = pp v <+> pp Gets <+> primDefaultVal ty <> semi
funVarReset (Decl (ConstVar _ty _n) _t _v) = empty
funVarReset _ = empty

process :: Ident -> FunBody -> SensitivityList -> Reader Network Doc
process fname body sensitivity = do
  -- XXX: Why not pass function from caller?
  vars <- fromMaybe [] <$> findPred functions (\s -> fname == funName s) locals
  fun <- fromMaybe [] <$> findPred functions (\s -> fname == funName s) funOutports
  sigResets <- vcat <$> mapM funSignalReset fun
  let varResets = vcat $ map funVarReset vars
  return (pp Process <+> parens ( empty $+$ sensitivity )
          -- TODO: Split definitions on constants and variables
          $+$ vcat (map makeVar vars)
          $+$ pp Begin
          $+$ indent (pp If <+> text "rst = '1'" <+> pp Then
                      $+$ indent (sigResets
                                  $+$ varResets)
                      $+$ pp Elsif <+> pp (RisingEdge (text "clk")) <+> pp Then
                      $+$ indent body
                      $+$ pp EndIf <> semi
                     )
          $+$ pp EndProcess <> semi)

instPortMap :: (Ident, Ident) -> Reader Network Doc
instPortMap ps = do
  bus <- findPred busses (\s -> snd ps == busName s) id
  asTopPorts <- fromMaybe (return []) (topBusPorts <$> bus)
  asFunPorts <- funPortNames ps
  return $ vcat $ map (\(a, b) -> a <+> pp MapTo <+> b <> comma) (zip asFunPorts (map fst asTopPorts))
  -- Format fun bus name => top lvl name

instParamsMap :: [(Ident, PrimVal)] -> Doc
-- FIXME: This will produce non-working VHDL code because generics without a
-- default value are left uninstantiated. Either set Decls with Nothing
-- expression to 0 by default or assign a default value to generics in
-- entities generated from external functions
instParamsMap ps = vcat $ commas $ filter (not.isEmpty) $ map (\(i, e) ->
                                          if e /= EmptyVal then
                                             text i <+> pp MapTo <+> pp e
                                          else empty) ps

inst :: Instance -> Reader Network Doc
inst Instance { instName = name
              , instFun = fun
              , inBusses = inbus
              , outBusses = outbus
              , instParams = params
              } = do
  funDef <- findPred functions (\n -> funName n == fun) id
  let funPorts = concat $ fromMaybe [] $ sequence [ toRealBus inbus . funInports <$> funDef
                                                  , toRealBus outbus . funOutports <$> funDef]
  portMaps <- vcat <$> mapM instPortMap funPorts
  let genMaps = instParamsMap params
  return $ text name <> colon <+> pp Entity <+> pp Work <> text "." <> text fun
    $+$ (if isEmpty genMaps then empty else pp GenericMap <+> parens genMaps)
    $+$ pp PortMap <+> parens (indent ( portMaps
                                        $+$ clockedMap
                                      )) <> semi
    where
      toRealBus bs ps = [(fst p, b) | (b, p) <- zip bs ps]

topBusPorts :: Bus -> Reader Network [(Doc, Doc)]
topBusPorts b = do
  nn <- asks netName
  let bn = busName b
  let bp = busPorts b
  return $ map (\(s, t) -> (underscores [nn, bn, s], pp t)) bp

topPorts :: Reader Network Doc
topPorts = do
  sigdefs <- asks busses >>= mapM topBusPorts
  return $ sigDefs $ concat sigdefs
  where
    sigDefs :: [(Doc, Doc)] -> Doc
    sigDefs = vcat . map (\(s, t) -> s  <> colon <+> pp InOut <+> t <> semi)

makeTopLevel :: Reader Network Doc
makeTopLevel = do
  nname <- asks netName
  ports <- topPorts
  insts <- asks instances >>= mapM inst
  return $ entity nname ports empty
    $+$ architecture nname (text "-- signals") (vcat insts)

tbSigDefs :: Reader Network [Doc]
tbSigDefs = do
  sigs <- asks busses >>= mapM topBusPorts
  return $ map (\(n, t) -> pp Signal <+> n <> colon <+> t <> semi) (concat sigs)

--tbSigMaps :: Reader Network [Doc]
--tbSigMaps 

tbBuss :: Reader Network [Doc]
tbBuss = do
  a <- asks busses >>= mapM topBusPorts
  -- HACK: We need trace file busses to be in alphabetical order to match trace
  -- CSV file header fields and Doc isn't an instance of Ord. We cant move this
  -- to the more general functions since alphabetical ordering of signals
  -- probably isn't ideal everywhere.
  return $ map text $ sort $ map (render . fst) $ concat a

tbSigMaps :: Reader Network [Doc]
tbSigMaps = map (\n -> n <+> pp MapTo <+> n <> comma) <$> tbBuss

makeTB :: Reader Network Doc
makeTB = do
  nn <- asks netName
  nntb <- (++ "_tb") <$> asks netName
  signals <- vcat <$> tbSigDefs
  buss <- tbBuss
  sigMaps <- tbSigMaps
  return (tbHeader
          $+$ pp Entity <+> pp nntb <+> pp Is
          $+$ pp End <+> pp nntb <> semi
          $+$ architecture nntb (signals $+$ tbSignals)
          ( pp "uut" <> colon <+> pp Entity <+> pp "work." <> pp nn
            $+$ pp PortMap <+> parens (
              indent ( vcat sigMaps
                       $+$ tbClockedMap
                     )
              ) <> semi
            $+$ clkProcess
            $+$ pp "TraceTester" <> colon <+> pp Process
            $+$ indent (testerDecls
                        $+$ pp "-- More decls here"
                       )
            $+$ pp Begin
            $+$ indent (pp (FileOpen (pp "Status") (pp "F") (pp "filename")) <> semi
                        $+$ pp If <+> pp "Status" <+> pp NeqOp <+> pp "OPEN_OK" <+> pp Then
                        $+$ indent (pp Report <+> pp "\"Unable to open trace file\"" <> semi)
                        $+$ pp Else
                        $+$ indent (pp (ReadLine (pp "F") (pp "L")) <> semi
                                    $+$ pp "fieldno" <+> pp Gets <+> pp "0" <> semi
                                    $+$ vcat (map fieldCheck buss)
                                    $+$ tbResetWait
                                    $+$ pp While <+> pp Not <+> pp (EndFile (pp "F")) <+> pp Loop
                                    $+$ indent (pp (ReadLine (pp "F") (pp "L")) <> semi
                                                $+$ pp Wait <+> pp Until <+> pp (RisingEdge (pp "clock")) <> semi
                                                $+$ pp "fieldno" <+> pp Gets <+> pp "0" <> semi
                                                $+$ vcat (map valCheck buss)
                                                $+$ pp "clockcycle" <+> pp Gets <+> pp "clockcycle" <+> pp PlusOp <+> pp "1" <> semi
                                              )
                                    $+$ pp EndLoop <> semi
                                    $+$ pp (FileClose (pp "F")) <> semi
                                   )
                        $+$ pp EndIf <> semi
                        $+$ pp Report <+> text "\"Completed after \" & integer'image(clockcycle) & \" clockcycles\"" <> semi
                        $+$ pp "stop_clock" <+> pp BusGets <+> pp True <> semi
                        $+$ pp Wait <> semi
                       )
            $+$ pp EndProcess <> semi
          )
         )

makeTypeDefs :: [DType] -> Doc
makeTypeDefs ts = typesHead
  $+$ pp Package <+> text "sme_types" <+> pp Is
  $+$ indent ((pp Subtype <+> text "bool_t" <+> pp Is <+> pp StdLogic <> semi)
              $+$ vcat (map typeDefs ts)
              $+$ vcat (map funDefs ts)
             )
  $+$ pp End <+> text "sme_types" <> semi
  $+$ text ""
  $+$ pp Package <+> pp Body <+> text "sme_types" <+> pp Is
  $+$ indent (vcat $ map bodyFuns ts)
  $+$ pp End <+> text "sme_types" <> semi
  where
    typeDefs :: DType -> Doc
    typeDefs t = pp Subtype <+> pp t <+> pp Is <+> pp (StdLogicVector (pp (sizeOf t - 1) <+> pp Downto <+> pp "0")) <> semi
    -- FIXME: Distinguish between integers and naturals (only convert from
      -- natural to unsigned and integer to signed)
    funDefs t = pp "" $+$ pp "-- converts an integer to" <+> typeName t
                $+$ pp PureFunction <+> typeName t <> parens (pp "v" <> colon <+> pp Integer)
                      <+> (pp Return <+> pp t <> semi)

    bodyFuns t = pp PureFunction <+> typeName t <> parens (text "v" <> colon
                                                                   <+> pp Integer)
      <+> pp Return <+> pp t <+> pp Is
      $+$ pp Begin
      $+$ indent (pp Return <+> pp (StdLogicVector (pp $ toSign (pp "v") (pp $ Length (pp t)))) <> semi)
      $+$ pp End <+> typeName t <> semi
      -- FIXME: Causes trailing spaces after newline
      $+$ pp ""
      where
        -- FIXME: This doesn't take into account floats, but we don't have any
        -- of those yet
        toSign :: Doc -> Doc -> VHDLFuns
        toSign d1 d2 = case sign t of
                         IsSigned -> ToSigned d1 d2
                         IsUnsigned -> ToUnsigned d1 d2

{-|

Toplevel: in entity, for every bus referenced by instances

-}

vhdlExt :: FilePath -> FilePath
vhdlExt = flip (++) ".vhdl"

makeFun :: Function -> Reader Network OutputFile
makeFun f = do
  let fname = funName f
  let generics = entGenerics f
  ports <- entPorts f
  procc <- process fname (pp (funBody f))(vcat [ text "clk" <> comma , text "rst" ])
  return OutputFile { dir = ""
                    , file = vhdlExt fname
                    , output = header
                      $+$ entity fname ports generics
                      $+$ architecture fname empty procc
                    }


makeNetwork :: [DType] -> Reader Network OutputPlan
makeNetwork ts = do
  nn <- asks netName
  funs <- asks functions
  outFuns <- mapM makeFun funs
  tl <- makeTopLevel
  tb <- makeTB
  return $ OutputFile { dir = ""
                      , file = vhdlExt nn
                      , output = header $+$ tl
                      }
    : OutputFile { dir = ""
                 , file = vhdlExt "csv_util"
                 , output = csvUtil
                 }
    : OutputFile { dir = ""
                 , file = vhdlExt "sme_types"
                 , output = makeTypeDefs ts
                 }
    : OutputFile { dir = ""
                 , file = vhdlExt $ nn ++ "_tb"
                 , output  = tb
                 }
    : outFuns

makePlan :: Network -> [DType] -> OutputPlan
makePlan n ts = runReader (makeNetwork ts) n

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
