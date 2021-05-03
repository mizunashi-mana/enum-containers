{- |
Module      :  Data.EnumSet

An implementation of enum sets to use IntSet internally.
See https://hackage.haskell.org/package/containers/docs/Data-IntSet.html
for more internal information.

Many operations have a worst-case complexity of /O(min(n,W))/.
This means that the operation can become linear in the number of
elements with a maximum of /W/ -- the number of bits in an 'Int'
(32 or 64).
-}


module Data.EnumSet (
    EnumSet,
    toIntSet,

    empty,
    singleton,

    null,
    size,
    member,
    notMember,
    lookupNearPred,
    lookupNearSucc,
    lookupEqOrNearPred,
    lookupEqOrNearSucc,

    insert,
    delete,

    union,
    difference,
    intersection,
    isProperSubsetOf,
    isSubsetOf,
    disjoint,

    filter,
    partition,
    splitEnumOrd,
    splitMemberEnumOrd,
    splitAnyway,
    map,
    foldl',
    foldr,
    toList,
    fromEnumOrderedList,

    maxBoundView,
    minBoundView,
    findMaxBound,
    findMinBound,
) where

import Prelude hiding (filter, null, map, foldr)

import qualified Control.DeepSeq             as DeepSeq
import qualified Data.IntSet                 as IntSet
import Data.Coerce


-- | A set of enums.
type role EnumSet nominal
newtype EnumSet a = UnsafeEnumSet IntSet.IntSet
    deriving (Eq, Ord, Show)

-- | /O(1)/. Convert a set of integers corresponding enums
-- of a given set.
toIntSet :: Enum a => EnumSet a -> IntSet.IntSet
toIntSet (UnsafeEnumSet is) = is
{-# INLINE toIntSet #-}

instance Semigroup (EnumSet a) where
    UnsafeEnumSet is1 <> UnsafeEnumSet is2 = UnsafeEnumSet (is1 <> is2)
    {-# INLINE (<>) #-}

instance Monoid (EnumSet a) where
    mempty = UnsafeEnumSet mempty
    {-# INLINE mempty #-}

instance DeepSeq.NFData (EnumSet a) where
    rnf (UnsafeEnumSet is) = DeepSeq.rnf is
    {-# INLINE rnf #-}


{-
-------------------------------------------------------------------
  Construction
-------------------------------------------------------------------
-}

-- | /O(1)/. The empty set.
empty :: EnumSet a
empty = UnsafeEnumSet IntSet.empty
{-# INLINE empty #-}

-- | /O(1)/. A set of one element.
singleton :: Enum a => a -> EnumSet a
singleton !x = UnsafeEnumSet (IntSet.singleton (fromEnum x))
{-# INLINE singleton #-}


{-
-------------------------------------------------------------------
  Lookup
-------------------------------------------------------------------
-}

-- | /O(1)/. Is the set empty?
null :: EnumSet a -> Bool
null (UnsafeEnumSet is) = IntSet.null is
{-# INLINE null #-}

-- | /O(n)/. Cardinality of the set.
size :: EnumSet a -> Int
size (UnsafeEnumSet is) = IntSet.size is
{-# INLINE size #-}

-- | /O(min(n,W))/. Is the value a member of the set?
member :: Enum a => a -> EnumSet a -> Bool
member !x (UnsafeEnumSet is) = IntSet.member (fromEnum x) is
{-# INLINE member #-}

-- | /O(min(n,W))/. Is the element not in the set?
notMember :: Enum a => a -> EnumSet a -> Bool
notMember !x (UnsafeEnumSet is) = IntSet.notMember (fromEnum x) is
{-# INLINE notMember #-}

-- | /O(log n)/. Find nearest element previous to the given one.
lookupNearPred :: Enum a => a -> EnumSet a -> Maybe a
lookupNearPred x (UnsafeEnumSet is) = case IntSet.lookupLT (fromEnum x) is of
    Nothing -> Nothing
    Just r  -> Just (toEnum r)
{-# INLINE lookupNearPred #-}

-- | /O(log n)/. Find nearest element next to the given one.
lookupNearSucc :: Enum a => a -> EnumSet a -> Maybe a
lookupNearSucc x (UnsafeEnumSet is) = case IntSet.lookupGT (fromEnum x) is of
    Nothing -> Nothing
    Just r  -> Just (toEnum r)
{-# INLINE lookupNearSucc #-}

-- | /O(log n)/. Find nearest element equal or previous to the given one.
lookupEqOrNearPred :: Enum a => a -> EnumSet a -> Maybe a
lookupEqOrNearPred x (UnsafeEnumSet is) = case IntSet.lookupLE (fromEnum x) is of
    Nothing -> Nothing
    Just r  -> Just (toEnum r)
{-# INLINE lookupEqOrNearPred #-}

-- | /O(log n)/. Find nearest element equal or next to the given one.
lookupEqOrNearSucc :: Enum a => a -> EnumSet a -> Maybe a
lookupEqOrNearSucc x (UnsafeEnumSet is) = case IntSet.lookupGE (fromEnum x) is of
    Nothing -> Nothing
    Just r  -> Just (toEnum r)
{-# INLINE lookupEqOrNearSucc #-}


{-
-------------------------------------------------------------------
  Insert
-------------------------------------------------------------------
-}

-- | /O(min(n,W))/. Add a value to the set. There is no left- or right bias for
-- EnumSets.
insert :: Enum a => a -> EnumSet a -> EnumSet a
insert !x (UnsafeEnumSet is) = UnsafeEnumSet (IntSet.insert (fromEnum x) is)
{-# INLINE insert #-}

-- | /O(min(n,W))/. Delete a value in the set. Returns the
-- original set when the value was not present.
delete :: Enum a => a -> EnumSet a -> EnumSet a
delete !x (UnsafeEnumSet is) = UnsafeEnumSet (IntSet.delete (fromEnum x) is)
{-# INLINE delete #-}


{-
-------------------------------------------------------------------
  Set Operators
-------------------------------------------------------------------
-}

-- | /O(n+m)/. The union of two sets.
union :: EnumSet a -> EnumSet a -> EnumSet a
union (UnsafeEnumSet is1) (UnsafeEnumSet is2) = UnsafeEnumSet (IntSet.union is1 is2)
{-# INLINE union #-}

-- | /O(n+m)/. Difference between two sets.
difference :: EnumSet a -> EnumSet a -> EnumSet a
difference (UnsafeEnumSet is1) (UnsafeEnumSet is2) = UnsafeEnumSet (IntSet.difference is1 is2)
{-# INLINE difference #-}

-- | /O(n+m)/. The intersection of two sets.
intersection :: EnumSet a -> EnumSet a -> EnumSet a
intersection (UnsafeEnumSet is1) (UnsafeEnumSet is2) = UnsafeEnumSet (IntSet.intersection is1 is2)
{-# INLINE intersection #-}

-- | /O(n+m)/. Is this a proper subset? (i.e. a subset but not equal).
isProperSubsetOf :: EnumSet a -> EnumSet a -> Bool
isProperSubsetOf (UnsafeEnumSet is1) (UnsafeEnumSet is2) = IntSet.isProperSubsetOf is1 is2
{-# INLINE isProperSubsetOf #-}

-- | /O(n+m)/. Is this a subset?
isSubsetOf :: EnumSet a -> EnumSet a -> Bool
isSubsetOf (UnsafeEnumSet is1) (UnsafeEnumSet is2) = IntSet.isSubsetOf is1 is2
{-# INLINE isSubsetOf #-}

-- | /O(n+m)/. Check whether two sets are disjoint (i.e. their intersection
--   is empty).
disjoint :: EnumSet a -> EnumSet a -> Bool
disjoint (UnsafeEnumSet is1) (UnsafeEnumSet is2) = IntSet.disjoint is1 is2
{-# INLINE disjoint #-}


{-
-------------------------------------------------------------------
  Container Operators
-------------------------------------------------------------------
-}

-- | /O(n)/. Filter all elements that satisfy some predicate.
filter :: Enum a => (a -> Bool) -> EnumSet a -> EnumSet a
filter p (UnsafeEnumSet is) =
    let ip i = p (toEnum i)
    in UnsafeEnumSet (IntSet.filter ip is)
{-# INLINE filter #-}

-- | /O(n)/. partition the set according to some predicate.
partition :: Enum a => (a -> Bool) -> EnumSet a -> (EnumSet a, EnumSet a)
partition p (UnsafeEnumSet is) =
    let ip i = p (toEnum i)
    in coerce (IntSet.partition ip is)
{-# INLINE partition #-}

-- | /O(min(n,W))/. split by the given value with the Enum order.
splitEnumOrd :: Enum a => a -> EnumSet a -> (EnumSet a, EnumSet a)
splitEnumOrd x (UnsafeEnumSet is) = coerce (IntSet.split (fromEnum x) is)
{-# INLINE splitEnumOrd #-}

-- | /O(min(n,W))/. split and member by the given value with the Enum order.
splitMemberEnumOrd :: Enum a => a -> EnumSet a -> (EnumSet a, Bool, EnumSet a)
splitMemberEnumOrd x (UnsafeEnumSet is) = coerce (IntSet.splitMember (fromEnum x) is)
{-# INLINE splitMemberEnumOrd #-}

-- | /O(1)/.  Decompose a set into pieces.
splitAnyway :: Enum a => EnumSet a -> [EnumSet a]
splitAnyway (UnsafeEnumSet is) = coerce (IntSet.splitRoot is)
{-# INLINE splitAnyway #-}

-- | /O(n*min(n,W))/. Build the set obtained by applying @f@
-- to each element of a given set.
map :: (Enum a, Enum b) => (a -> b) -> EnumSet a -> EnumSet b
map f (UnsafeEnumSet is) =
    let if' i = fromEnum (f (toEnum i))
    in UnsafeEnumSet (IntSet.map if' is)
{-# INLINE map #-}

-- | /O(n)/. Fold the elements in the set using the given left-associative
-- binary operator. Each application of the operator is evaluated before
-- using the result in the next application.
foldl' :: Enum a => (b -> a -> b) -> b -> EnumSet a -> b
foldl' f z0 (UnsafeEnumSet is) =
    let if' z i = f z (toEnum i)
    in IntSet.foldl' if' z0 is
{-# INLINE foldl' #-}

-- | /O(n)/. Fold the elements in the set using the given right-associative
-- binary operator.
foldr :: Enum a => (a -> b -> b) -> b -> EnumSet a -> b
foldr f z0 (UnsafeEnumSet is) =
    let if' i z = f (toEnum i) z
    in IntSet.foldr if' z0 is
{-# INLINE foldr #-}

-- | /O(n)/. Convert the set to a list of elements. Subject to list fusion.
toList :: Enum a => EnumSet a -> [a]
toList (UnsafeEnumSet is) = [ toEnum i | i <- IntSet.toList is ]
{-# INLINE toList #-}

-- | /O(n)/. Build a set from an ascending enum list of elements.
-- /The precondition (input list is ascending) is not checked./
fromEnumOrderedList :: Enum a => [a] -> EnumSet a
fromEnumOrderedList l = UnsafeEnumSet (IntSet.fromAscList [ fromEnum e | e <- l ])
{-# INLINE fromEnumOrderedList #-}


{-
-------------------------------------------------------------------
  Min/Max
-------------------------------------------------------------------
-}

-- | /O(min(n,W))/. Retrieves the maximal bound of the set, and the set
-- stripped of that element, or 'Nothing' if passed an empty set.
maxBoundView :: Enum a => EnumSet a -> Maybe (a, EnumSet a)
maxBoundView (UnsafeEnumSet is) = case IntSet.maxView is of
    Nothing     -> Nothing
    Just (x, r) -> Just (toEnum x, UnsafeEnumSet r)
{-# INLINE maxBoundView #-}

-- | /O(min(n,W))/. Retrieves the minimal bound of the set, and the set
-- stripped of that element, or 'Nothing' if passed an empty set.
minBoundView :: Enum a => EnumSet a -> Maybe (a, EnumSet a)
minBoundView (UnsafeEnumSet is) = case IntSet.minView is of
    Nothing     -> Nothing
    Just (x, r) -> Just (toEnum x, UnsafeEnumSet r)
{-# INLINE minBoundView #-}

-- | /O(min(n,W))/. The maximal bound of the set or 'Nothing'
-- if passed an empty set.
findMaxBound :: Enum a => EnumSet a -> Maybe a
findMaxBound (UnsafeEnumSet is)
    | IntSet.null is    = Nothing
    | otherwise         = Just (toEnum (IntSet.findMax is))
{-# INLINE findMaxBound #-}

-- | /O(min(n,W))/. The minimal bound of the set or 'Nothing'
-- if passed an empty set.
findMinBound :: Enum a => EnumSet a -> Maybe a
findMinBound (UnsafeEnumSet is)
    | IntSet.null is    = Nothing
    | otherwise         = Just (toEnum (IntSet.findMin is))
{-# INLINE findMinBound #-}
