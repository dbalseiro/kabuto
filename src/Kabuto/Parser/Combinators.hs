{-# LANGUAGE OverloadedStrings #-}

module Kabuto.Parser.Combinators (typeDescriptorsP) where

import Kabuto.Parser.Types

import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Data.Text as T
import Data.Text (Text)
import Data.Void (Void)

import Control.Monad (void)

type Parser = Parsec Void Text


typeDescriptorsP :: Parser [TypeDescriptor]
typeDescriptorsP = do
  filler
  many (typeDefP <|> unionDefP <|> namedDefP <* filler) <* eof

filler :: Parser ()
filler = space

typeDefP :: Parser TypeDescriptor
typeDefP = do
  void $ try (string "type")
  filler
  name <- typeNameP
  filler
  fields <- between (char '{') (char '}') $ do
    filler
    sepEndBy fieldDefP eol <* filler

  filler
  return $ TypeDescriptor name (ProductTypeDef fields)

typeNameP :: Parser TypeName
typeNameP = TypeName . T.pack <$> nameP upperChar

fieldNameP :: Parser FieldName
fieldNameP = FieldName . T.pack <$> nameP lowerChar

nameP :: Parser Char -> Parser String
nameP p = (:) <$> p <*> many (alphaNumChar <|> char '_')

fieldDefP :: Parser FieldDef
fieldDefP = do
  hspace
  fname <- fieldNameP
  void $ char ':'
  hspace
  ftype <- fieldTypeP
  hspace
  return $ FieldDef fname ftype

fieldTypeP :: Parser FieldType
fieldTypeP = listFieldTypeP
         <|> BaseType <$> try baseTypeP
         <|> ScalarType <$> typeNameP

baseTypeP :: Parser Primitive
baseTypeP = StringType <$ string "String"
        <|> IntType    <$ string "Integer"
        <|> BoolType   <$ string "Boolean"

listFieldTypeP :: Parser FieldType
listFieldTypeP = ListType <$> between (try $ char '[') (char ']') typeNameP

unionDefP :: Parser TypeDescriptor
unionDefP = do
  void $ try (string "union")
  filler
  name <- typeNameP
  hspace
  void $ char '='
  filler
  typs <- sepBy1 (filler *> fieldTypeP <* filler) (char '|')
  return $ TypeDescriptor name (SumTypeDef typs)

namedDefP :: Parser TypeDescriptor
namedDefP = do
  void $ try (string "named")
  hspace
  name <- typeNameP
  hspace
  TypeDescriptor name . NamedTypeDef <$> fieldTypeP

