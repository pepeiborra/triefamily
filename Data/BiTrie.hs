{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

----------------------------------------------------------------------
-- |
-- Module      :  Data.BiTrie
-- Copyright   :  (c) Jose Iborra 2009
-- License     :  BSD3
-- 
-- Maintainer  :  pepeiborra@gmail.com
-- Stability   :  experimental
-- 
----------------------------------------------------------------------



module Data.BiTrie (
  HasTrie,
  (:<->:),
  lookup,
  lookupR,
  insert,
  toList,
  elems,
  size
 ) where

import Data.Monoid
import Data.TrieFamily (HasTrie, (:->:))
import qualified Data.TrieFamily as Trie
import Prelude hiding (lookup)

data a :<->: b = BiTrie (a :->: b) (b :->: a) deriving (Eq, Ord, Show)

lookup :: HasTrie a => a -> a :<->: b -> Maybe b
lookup a (BiTrie t _) = Trie.lookup a t

lookupR :: HasTrie b => b -> a :<->: b -> Maybe a
lookupR b (BiTrie _ tr) = Trie.lookup b tr

insert :: (HasTrie a, HasTrie b) => a -> b -> a :<->: b -> a :<->: b
insert a b (BiTrie t tr) = BiTrie (Trie.insert a b t)
                                  (Trie.insert b a tr)

toList :: HasTrie a => a :<->: b -> [(a,b)]
toList (BiTrie a _) = Trie.toList a

elems  :: HasTrie a => a :<->: b -> [b]
elems = map snd . toList

size (BiTrie a b) = Trie.size a

instance (HasTrie a, HasTrie b) => Monoid (a :<->: b) where
  mempty = BiTrie Trie.empty Trie.empty
  mappend (BiTrie a b) (BiTrie a' b') = BiTrie (mappend a a') (mappend b b')
