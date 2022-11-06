module Parser (parseModuleIO) where

import Parser.Types (TypeDescriptor)
import Parser.Combinators (typeDescriptorsP)

import Data.Text (Text)
import qualified Data.Text.IO as T

import Control.Exception (throwIO, Exception)
import Control.Arrow (left)

import Text.Megaparsec

newtype ParseException = ParseException String
  deriving Show
instance Exception ParseException

parseModuleIO :: FilePath -> IO [TypeDescriptor]
parseModuleIO f =
  T.readFile f >>= either (throwIO . ParseException) return . parseModule f

parseModule :: FilePath -> Text -> Either String [TypeDescriptor]
parseModule f = left errorBundlePretty . parse typeDescriptorsP f

