module Parser.Combinators (typeDescriptorsP) where

import Parser.Types

import Text.Megaparsec
import Text.Megaparsec.Char

import Data.Void (Void)
import Data.Text (Text)

import Control.Monad (void)

type Parser = Parsec Void Text


typeDescriptorsP :: Parser [TypeDescriptor]
typeDescriptorsP = some $
  filler *> typeDefP

filler :: Parser ()
filler = undefined

typeDefP :: Parser TypeDescriptor
typeDefP = do
  void $ try (string "type")
  space
  name <- typeNameP
  space
  void $ char '{'
  space
  fields <- many fieldDefP
  space
  void $ char '}'
  return $ TypeDescriptor name (ProductTypeDef fields)

typeNameP :: Parser TypeName
typeNameP = undefined

fieldDefP :: Parser FieldDef
fieldDefP = undefined

