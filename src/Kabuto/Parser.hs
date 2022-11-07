module Kabuto.Parser (compileTypeIO, ParseException(..)) where

import Kabuto.Parser.Types
import Kabuto.Schema.Types
import Kabuto.Schema
import Kabuto.Parser.Combinators (typeDescriptorsP)

import Data.Fix (Fix)
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.HashMap.Strict as M

import Control.Exception (throwIO, Exception)
import Control.Arrow (left)
import Control.Monad (foldM, (>=>))

import Text.Megaparsec

newtype ParseException = ParseException String
  deriving Show
instance Exception ParseException

compileTypeIO :: FilePath -> TypeName -> IO (Fix SchemaDefF)
compileTypeIO f tname = T.readFile f >>= throwLeft . compile
  where
    compile :: Text -> Either String (Fix SchemaDefF)
    compile = parseModule >=> typeSystem >=> generateSchemaDef tname

    throwLeft :: Either String a -> IO a
    throwLeft = either (throwIO . ParseException) return

    parseModule :: Text -> Either String [TypeDescriptor]
    parseModule = left errorBundlePretty . parse typeDescriptorsP f

    typeSystem :: [TypeDescriptor] -> Either String TypeSystem
    typeSystem = foldM safeInsert M.empty
      where
        safeInsert m (TypeDescriptor typeName typeDef) = case M.lookup typeName m of
          Nothing -> return (M.insert typeName typeDef m)
          Just _ -> Left $ "Duplicated type name: " ++ show typeName

