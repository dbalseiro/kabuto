{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kabuto.Parser.Types
  ( TypeDescriptor (..)
  , TypeName (..)
  , FieldName (..)
  , FieldDef (..)
  , Primitive (..)
  , TypeDef (..)
  , FieldType (..)
  ) where

import Data.Text (Text)
import Data.Hashable (Hashable)

data TypeDescriptor = TypeDescriptor TypeName TypeDef deriving (Eq, Show)

data TypeDef
  = ProductTypeDef [FieldDef]
  | SumTypeDef [FieldType]
  | NamedTypeDef FieldType
  deriving (Eq, Show)

data Primitive
  = StringType
  | IntType
  | BoolType
  deriving (Eq, Show)

newtype TypeName = TypeName { unTypeName :: Text } deriving (Eq, Show, Hashable)
newtype FieldName = FieldName { unFieldName :: Text } deriving (Eq, Show)

data FieldDef = FieldDef
  { fieldName :: FieldName
  , fieldType :: FieldType
  }
  deriving (Eq, Show)

data FieldType = ScalarType TypeName | BaseType Primitive | ListType FieldType
  deriving (Eq, Show)
