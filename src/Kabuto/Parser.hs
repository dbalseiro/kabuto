module Kabuto.Parser (parseModuleIO, ParseException(..)) where

import Kabuto.Parser.Types (TypeDescriptor (TypeDescriptor), TypeName, TypeDef)
import Kabuto.Parser.Combinators (typeDescriptorsP)

import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M

import Control.Exception (throwIO, Exception)
import Control.Arrow (left)
import Control.Monad (foldM, (>=>))

import Text.Megaparsec

newtype ParseException = ParseException String
  deriving Show
instance Exception ParseException

parseModuleIO :: FilePath -> IO (HashMap TypeName TypeDef)
parseModuleIO f =
  T.readFile f >>= either (throwIO . ParseException) return . (parseModule f >=> typeSystem)

parseModule :: FilePath -> Text -> Either String [TypeDescriptor]
parseModule f = left errorBundlePretty . parse typeDescriptorsP f

typeSystem :: [TypeDescriptor] -> Either String (HashMap TypeName TypeDef)
typeSystem = foldM alg M.empty
  where
    alg :: HashMap TypeName TypeDef -> TypeDescriptor -> Either String (HashMap TypeName TypeDef)
    alg m (TypeDescriptor typeName typeDef) =
      case M.lookup typeName m of
        Nothing -> return (M.insert typeName typeDef m)
        Just _ -> Left $ "Duplicated type name: " ++ show typeName

