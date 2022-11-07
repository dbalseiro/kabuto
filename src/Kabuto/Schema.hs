module Kabuto.Schema (generateSchemaDef, printSchema) where

import Kabuto.Schema.PrettyPrint
import Kabuto.Schema.Types
import Kabuto.Parser.Types

import Data.Fix
import qualified Data.HashMap.Strict as M
import Data.HashSet (HashSet)
import qualified Data.HashSet as S

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (State, evalState, get, modify)

data Seed = TName TypeName | TPrimitive Primitive

generateSchemaDef :: TypeName -> TypeSystem -> Either String (Fix SchemaDefF)
generateSchemaDef t typeSystem = flip evalState mempty . runExceptT $ unfoldFixM coalg (TName t)
  where
    coalg :: Seed -> ExceptT String (State (HashSet TypeName)) (SchemaDefF Seed)
    coalg (TName typeName) = do
      s <- get
      if typeName `S.member` s
         then throwError (show typeName ++ " Cyclic type")
         else modify (S.insert typeName)

      case M.lookup typeName typeSystem of
        Nothing -> throwError (show typeName ++ " Not Found")
        Just tdef -> return (processTypeDef typeName tdef)

    coalg (TPrimitive primitive) = return (BaseDef primitive)

    processTypeDef :: TypeName -> TypeDef -> SchemaDefF Seed
    processTypeDef nm (NamedTypeDef ft) = processFieldType nm ft
    processTypeDef nm (SumTypeDef fts) = SumDefF nm $ fmap getSeed fts
    processTypeDef nm (ProductTypeDef fds) = ProductDefF nm $ fmap getSeedFromDef fds

    processFieldType :: TypeName -> FieldType -> SchemaDefF Seed
    processFieldType _ (BaseType primitive) = BaseDef primitive
    processFieldType nm (ScalarType tn) = ScalarDefF nm (TName tn)
    processFieldType _ (ListType tn) = ListDefF (TName tn)

    getSeed :: FieldType -> Seed
    getSeed (BaseType primitive) = TPrimitive primitive
    getSeed (ScalarType tn) = TName tn
    getSeed (ListType tn) = TName tn

    getSeedFromDef :: FieldDef -> (FieldName, Seed)
    getSeedFromDef (FieldDef fname ftype) = (fname, getSeed ftype)


printSchema :: Fix SchemaDefF -> IO ()
printSchema = putStrLn . pprint


