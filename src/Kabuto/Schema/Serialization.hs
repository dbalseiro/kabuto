{-# LANGUAGE OverloadedStrings #-}
module Kabuto.Schema.Serialization (defaultValue) where

import Kabuto.Schema.Types

import Data.Fix
import qualified Data.Vector as V
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Kabuto.Parser (compileTypeIO)
import Kabuto.Parser.Types
import Kabuto.Schema.PrettyPrint

defaultValue :: Fix SchemaDefF -> A.Value
defaultValue = foldFix alg where
  alg :: SchemaDefF A.Value -> A.Value
  alg (BaseDef IntType) = A.Number 0
  alg (BaseDef StringType) = A.String ""
  alg (BaseDef BoolType) = A.Bool False
  alg (ProductDefF _ fields) = A.object (map toAesonField fields)
  alg (SumDefF _ (value:_)) = value
  alg (ListDefF value) = A.Array (V.singleton value)
  alg (ScalarDefF _ value) = value

toAesonField :: (FieldName, A.Value) -> A.Pair
toAesonField (FieldName txt, value) = txt A..= value

test :: IO ()
test = do
  schemadef <- compileTypeIO "fixtures/sample.schema" (TypeName "Student")
  print (A.encode $ defaultValue schemadef)

