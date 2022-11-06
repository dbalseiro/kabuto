module Parser.Types
  ( TypeDescriptor (..)
  , TypeName (..)
  , FieldName (..)
  , FieldDef (..)
  , BaseType (..)
  , TypeDef (..)
  ) where

import Data.Text (Text)

data TypeDescriptor = TypeDescriptor TypeName TypeDef deriving Show

data TypeDef
  = ProductTypeDef [FieldDef]
  | SumTypeDef [TypeName]
  | ListTypeDef TypeName
  | NamedTypeDef TypeName
  | BaseTypeDef BaseType
  deriving Show

data BaseType
  = StringType
  | IntType
  | BoolType
  deriving Show

newtype TypeName = TypeName { unTypeName :: Text } deriving Show
newtype FieldName = FieldName { unFieldName :: Text } deriving Show

data FieldDef = FieldDef
  { fieldName :: FieldName
  , fieldType :: TypeName
  }
  deriving Show

