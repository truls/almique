module Almique.CommentSupport
  ( lineMap
  , parseVarAnnot
  , parseBusAnnot
  )
  where

import qualified Data.IntMap as IntMap
import Data.Maybe (mapMaybe)

import Text.Parsec
import Text.Parsec.String

import Language.Python.Common.Token
import Language.Python.Common.SrcLocation

import Language.SMEIL.AST

type LineMap = IntMap.IntMap String

lineMap :: [Token] -> LineMap
lineMap = IntMap.fromList . mapMaybe tokMap
  where
    tokMap CommentToken
      { token_span = s
      , token_literal = c
      } = Just (startRow s, c)
    tokMap _ = Nothing

parseVarAnnot :: String -> Maybe DType
parseVarAnnot s = case parse typeParser "" s of
  Left _ -> Nothing
  Right r -> Just r

parseBusAnnot :: String -> Maybe DType
parseBusAnnot s = case parse typeNameParser "" s of
  Left _ -> Nothing
  Right r -> Just r

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

typeTok :: Parser ()
typeTok = lexeme (char '#')
  >> lexeme (string "type")
  >> lexeme (char ':')
  -- FIXME: This should be configurable
  >> lexeme (string "t.")
  >> pure ()

typeParser :: Parser DType
typeParser = typeTok >> typeNameParser

typeNameParser :: Parser DType
typeNameParser = choice [ boolParser
                        , signedParser
                        , unsignedParser
                        ]

boolParser :: Parser DType
boolParser = char 'b' >> pure BoolType

number :: Parser Int
number = read <$> many1 digit

signedParser :: Parser DType
signedParser = char 'i' *> (IntType <$> number)

unsignedParser :: Parser DType
unsignedParser = char 'u' *> (UIntType <$> number)
