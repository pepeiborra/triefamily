{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

----------------------------------------------------------------------
-- |
-- Module      :  Data.TrieFamily
-- Copyright   :  (c) Jose Iborra 2009
-- License     :  BSD3
-- 
-- Maintainer  :  pepeiborra@gmail.com
-- Stability   :  experimental
-- 
-- Type Families based Tries
-- Inspired by Hinze generalized maps, Spencer Janssen lazy tries,
-- and Conal Elliot's MemoTrie package
----------------------------------------------------------------------



module Data.TrieFamily (HasTrie(..)) where

import Control.Arrow
import Control.Monad
import Data.Char (chr, ord)
import Data.Map (Map)
import Data.IntMap  (IntMap)
import Data.Maybe
import Data.Monoid
import Prelude hiding (lookup)

import qualified Data.IntMap as IntMap
import qualified Data.Map as Map


class HasTrie a where
  data (:->:) a :: * -> *
  empty  :: a :->: b
  lookup :: a -> (a :->: b) -> Maybe b
  insert :: a -> b -> a :->: b -> a :->: b
  toList :: a :->: b -> [(a,b)]

instance HasTrie a => Monoid (a :->: x) where
  mempty = empty
  mappend ma mb = foldr (uncurry insert) ma (toList mb)

instance (HasTrie a, Eq a, Eq b) => Eq (a :->: b) where
  ta == tb = toList ta == toList tb

instance (HasTrie a, Ord a, Ord b) => Ord (a :->: b) where
  compare ta tb = compare (toList ta) (toList tb)

instance (HasTrie a, Show a, Show b) => Show (a :->: b) where
  showsPrec p ta = showsPrec p "fromList " . showsPrec p (toList ta)

fromList :: HasTrie a => [(a,b)] -> a :->: b
fromList = foldr (uncurry insert) empty

instance HasTrie Bool where
  data Bool :->: x = BoolTrie !(Maybe x) !(Maybe x)
  empty = BoolTrie Nothing Nothing
  lookup False (BoolTrie f _) = f
  lookup True  (BoolTrie _ t) = t
  insert False v (BoolTrie t _) = BoolTrie t (Just v)
  insert True  v (BoolTrie _ f) = BoolTrie (Just v) f
  toList (BoolTrie t f) = catMaybes [fmap ((,) True) t, fmap ((,) False) f]

instance HasTrie Char where
  newtype (Char :->: x) = CharTrie (IntMap x)
  empty = CharTrie $ IntMap.empty
  lookup a (CharTrie m) = IntMap.lookup (ord a) m
  insert k v (CharTrie m) = CharTrie $ IntMap.insert (ord k) v m
  toList (CharTrie m) = map (first chr) (IntMap.toList m)

instance HasTrie Int where
  newtype Int :->: x = IntTrie (IntMap x)
  empty = IntTrie mempty
  lookup a (IntTrie m)   = IntMap.lookup a m
  insert k v (IntTrie m) = IntTrie $ IntMap.insert k v m
  toList (IntTrie m)     = IntMap.toList m

instance HasTrie a => HasTrie (Maybe a) where
  data Maybe a :->: x = MaybeTrie !(Maybe x) !(a :->: x)
  empty = MaybeTrie Nothing empty
  lookup Nothing (MaybeTrie n _) = n
  lookup (Just a) (MaybeTrie _ jm) = lookup a jm
  insert Nothing  v (MaybeTrie _ jm)  = MaybeTrie (Just v) jm
  insert (Just k) v (MaybeTrie nm jm) = MaybeTrie nm (insert k v jm)
  toList (MaybeTrie Nothing   jm) = map (first Just) (toList jm)
  toList (MaybeTrie (Just nv) jm) = (Nothing, nv) : map (first Just) (toList jm)

instance (HasTrie a, HasTrie b) => HasTrie (Either a b) where
  data Either a b :->: x = EitherTrie !(a :->: x) !(b :->: x)
  empty = EitherTrie empty empty
  lookup (Left l)  (EitherTrie lt _) = lookup l lt
  lookup (Right r) (EitherTrie _ rt) = lookup r rt
  insert (Left l) v  (EitherTrie lt rt) = EitherTrie (insert l v lt) rt
  insert (Right r) v (EitherTrie lt rt) = EitherTrie lt (insert r v rt)
  toList (EitherTrie lt rt) = map (first Left)  (toList lt) ++
                              map (first Right) (toList rt)

instance (HasTrie a, HasTrie b) => HasTrie (a,b) where
  newtype (a,b) :->: x = TupleTrie (a :->: (b :->: x))
  empty = TupleTrie empty
  lookup (a,b) (TupleTrie m) = lookup a m >>= lookup b
  insert (a,b) v (TupleTrie m)
      = TupleTrie $ insert a (insert b v (fromMaybe empty$ lookup a m)) m
  toList (TupleTrie ma) = [ ((a,b),v) | (a, mb) <- toList ma
                                      , (b, v) <- toList mb]

instance (HasTrie a, HasTrie b, HasTrie c) => HasTrie (a,b,c) where
  newtype (a,b,c) :->: x = TripleTrie (a :->: (b :->: (c :->: x)))
  empty = TripleTrie empty
  lookup (a,b,c) (TripleTrie m) = lookup a m >>= lookup b >>= lookup c
  insert (a,b,c) v (TripleTrie m)
      = TripleTrie $ insert a
                            (let bt = fromMaybe empty$ lookup a m
                             in insert b
                                       (insert c v (fromMaybe empty $ lookup b bt))
                                       bt)
                            m
  toList (TripleTrie ma) = [ ((a,b,c),v) | (a, mb) <- toList ma
                                         , (b, mc) <- toList mb
                                         , (c, v ) <- toList mc]

instance HasTrie () where
  newtype () :->: b = UnitTrie (Maybe b)
  empty = UnitTrie Nothing
  lookup () (UnitTrie x) = x
  insert () x _ = UnitTrie (Just x)
  toList (UnitTrie Nothing) = []
  toList (UnitTrie (Just x))= [((),x)]

instance HasTrie a => HasTrie [a] where
  newtype [a] :->: x = ListTrie (Either () (a,[a]) :->: x)
  empty = ListTrie empty
  lookup k (ListTrie t) = lookup (delist k) t
  insert k v (ListTrie t) = ListTrie $ insert (delist k) v t
  toList (ListTrie m) = enum' list m

list = const [] `either` uncurry (:)
delist []     = Left  ()
delist (x:xx) = Right (x,xx)

-- Extracted from Data.MemoTrie
-- (c) Conal Elliott 2008
enum' :: (HasTrie a) => (a -> a') -> (a :->: b) -> [(a', b)]
enum' f = (fmap.first) f . toList

