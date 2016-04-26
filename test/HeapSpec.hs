module HeapSpec
  ( heapTests )
where

import Data.Maybe
import Test.Tasty
import Test.Tasty.QuickCheck

import Lang.Cortho.Heap (Heap, Addr)
import qualified Lang.Cortho.Heap as H


-- Test Group ----------------------------------------------------------

heapTests = testGroup "Heap Tests"
  [ testProperty "alloc" $ test_alloc
  , testProperty "remove" $ test_remove
  , testProperty "update" $ test_update
  , testProperty "addresses membership" $ test_addresses
  ]


-- Properties ----------------------------------------------------------

-- | Allocating an object on an existing heap produces one that contains the
-- object. Allocation always increases heap size.
test_alloc :: Heap Int -> Int -> Bool
test_alloc h x = isThere && H.size h' == H.size h + 1
  where
    (h', a) = H.alloc h x
    isThere = case H.lookup h' a of
                Nothing -> False
                Just _  -> True

-- | Removing an object at a valid address reduces the heap size. Removing an
-- object at an invalid address does nothing to the size.
test_remove :: Heap Int -> Bool
test_remove h = H.lookup h' a == Nothing
  where
    a = head (H.addresses h)
    h' = H.free h a

-- | Updating a heap at a valid address does update the object contained
-- there.
test_update :: Heap Int -> Int -> Bool
test_update h x = maybe False (\hh -> H.lookup hh a == Just x) h'
  where
    a = head (H.addresses h)
    h' = H.update h a x

-- | The address returned by an allocation is a member of the addresses list.
test_addresses :: Heap Int -> Int -> Bool
test_addresses h obj =
  let (h', a) = H.alloc h obj
  in  a `elem` H.addresses h'

-- Arbitrary Instances -------------------------------------------------

-- | Produce an arbitrary (non-empty) heap by allocating a sequence of random
-- objects on the empty heap.
instance Arbitrary a => Arbitrary (Heap a) where
  arbitrary = do
    xs <- listOf1 arbitrary  -- arbitrary objects
    return $ foldl H.alloc' H.initial xs

