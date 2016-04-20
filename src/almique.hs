module Main (main) where

import System.Environment
import System.Exit
-- import Control.Monad.IO.Class

--import Language.SMEIL
import Almique.Analyzer
import Almique.Binder

import Language.Python.Common as Py
import Language.Python.Version3 as Py3

-- Parsing or analysis error
data Error = GenErr AnError | ParseErr ParseError
             deriving Show

parseFile :: FilePath -> IO (Either Error (AnState, AnLog))
parseFile path = do
    contents <- readFile path
    case Py3.parseModule contents path of
      Left e -> return $ Left $ ParseErr e
      Right (m, _c) ->
        case analyzePyMod m of
          Left e -> return $ Left $ GenErr e
          Right r ->  return $ Right r

main :: IO ()
main = do
  args <- getArgs
  let path = case args of
        [fname] -> Just fname
        _ -> Nothing

  case path of
    Just p -> do
      res <- parseFile p
      case res of
        Left _ -> do
          print res
          -- FIXME: return error code appears not to be working
          exitWith $ ExitFailure 1
        Right network ->
          print network
    Nothing -> do
        putStrLn "Usage: almique <file.py>"
        exitWith $ ExitFailure 1
  return ()
