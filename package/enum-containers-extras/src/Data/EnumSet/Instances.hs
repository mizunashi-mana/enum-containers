{- |
Module      :  Data.EnumSet.Instances

This module provide some orphan instances for Data.EnumSet.
-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE TypeFamilies #-}

module Data.EnumSet.Instances () where

import qualified Data.EnumSet as EnumSet
import qualified Data.Hashable as Hashable
import qualified Data.IntSet as IntSet
import qualified Data.MonoTraversable as MonoTraversable


instance Enum a => Hashable.Hashable (EnumSet.EnumSet a) where
    hashWithSalt s es =
        Hashable.hashWithSalt s (IntSet.toAscList (EnumSet.toIntSet es))


type instance MonoTraversable.Element (EnumSet.EnumSet a) = a

instance Enum a => MonoTraversable.MonoFunctor (EnumSet.EnumSet a) where
    omap = EnumSet.map

instance Enum a => MonoTraversable.MonoFoldable (EnumSet.EnumSet a) where
    ofoldr = EnumSet.foldr
    ofoldl' = EnumSet.foldl'
    onull = EnumSet.null
    olength = EnumSet.size
    otoList = EnumSet.toList
    oelem = EnumSet.member
    onotElem = EnumSet.notMember

    ofoldMap f es = EnumSet.foldl' (\z x -> z <> f x) mempty es

    ofoldr1Ex f es = MonoTraversable.ofoldr1Ex f (EnumSet.toList es)

    ofoldl1Ex' f es = MonoTraversable.ofoldl1Ex' f (EnumSet.toList es)

instance Enum a => MonoTraversable.MonoPointed (EnumSet.EnumSet a) where
    opoint = EnumSet.singleton

instance Enum a => MonoTraversable.GrowingAppend (EnumSet.EnumSet a)
