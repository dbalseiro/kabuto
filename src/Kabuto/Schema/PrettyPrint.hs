{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Kabuto.Schema.PrettyPrint (pprint) where

import Kabuto.Schema.Types

import Data.Foldable (toList)
import Data.Fix
import Data.Tree (Tree)
import qualified Data.Tree as Tr

toTree :: forall f b . (Functor f, Foldable f) => (forall a . f a -> b) -> Fix f -> Tree b
toTree fxn = foldFix alg
  where
    alg :: f (Tree b) -> Tree b
    alg fx = Tr.Node (fxn fx) (toList fx)

pprint :: (Foldable f, Functor f, ShowNode f) => Fix f -> String
pprint = Tr.drawTree . toTree showNode

