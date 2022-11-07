{-# LANGUAGE DeriveTraversable #-}

module Kabuto.Schema.Types (SchemaDefF(..), ShowNode(..), TypeSystem) where

import Kabuto.Parser.Types

import Data.HashMap.Strict (HashMap)
import qualified Data.Text as T

type TypeSystem = HashMap TypeName TypeDef

data SchemaDefF f
  = ProductDefF TypeName [(FieldName, f)]
  | SumDefF TypeName [f]
  | ListDefF f
  | ScalarDefF TypeName f
  | BaseDef Primitive
  deriving (Functor, Foldable, Traversable)

class ShowNode f where
  showNode :: f a -> String

instance ShowNode SchemaDefF where
  showNode (ProductDefF tn l) = T.unpack $ unTypeName tn <> "{" <> T.intercalate ", " (map (unFieldName . fst) l) <> "}"
  showNode (SumDefF tn _) = fromTypeName tn ++ "|"
  showNode (ListDefF _) = "[]"
  showNode (ScalarDefF tn _) = fromTypeName tn ++ "@"
  showNode (BaseDef StringType) = "String"
  showNode (BaseDef IntType) = "Integer"
  showNode (BaseDef BoolType) = "Boolean"

fromTypeName :: TypeName -> String
fromTypeName = T.unpack . unTypeName



