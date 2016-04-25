module HeapSpec where

import Test.Tasty
import Test.Tasty.QuickCheck

import Lang.Cortho.Heap (Heap, Addr)
import qualified Lang.Cortho.Heap as H

-- Test Group ----------------------------------------------------------

heapTests = testGroup "Heap Tests"
  [ testProperty "alloc" $ test_alloc
  , testProperty "alloc size" $ test_alloc_size
  , testProperty "remove" $ test_remove
  , testProperty "update" $ test_update
  ]


-- Properties ----------------------------------------------------------

-- | Allocating an object on an existing heap produces one that contains the
-- object.
test_alloc :: Heap Int -> Int -> Bool
test_alloc h x =
    case H.lookup h' addr of
      Nothing -> False
      Just _  -> True
  where (h', addr) = H.alloc h x

-- | Allocation increases size.
test_alloc_size :: Heap Int -> Int -> Bool
test_alloc_size h x = H.size h' == H.size h + 1
  where
    h' = H.alloc' h x

-- | Removing an object at a valid address reduces the heap size. Removing an
-- object at an invalid address does nothing to the size.
test_remove :: Heap Int -> Addr -> Bool
test_remove ini a =
    let h' = H.free ini a
    in H.lookup h' a == Nothing &&
       case H.lookup ini a of
         Nothing -> H.size ini == H.size h'
         Just _  -> H.size ini == H.size h' + 1

-- | Updating a heap at a valid address does update the object contained
-- there.
test_update :: Heap Int -> Int -> Bool
test_update h x = maybe False (\hh -> H.lookup hh addr == Just x) h'
  where
    addr = head (H.addresses h)
    h' = H.update h addr x

-- Arbitrary Instances -------------------------------------------------

-- | Produce arbitary addresses from arbitrary non-negative integers.
instance Arbitrary Addr where
  arbitrary = H.mkAddr <$> arbitrary `suchThat` (>=0)

-- | Produce an arbitrary non-empty heap by allocating a sequence of random objects on
-- the empty heap.
instance Arbitrary a => Arbitrary (Heap a) where
  arbitrary = do
    xs <- listOf1 arbitrary  -- arbitrary objects
    return $ foldl H.alloc' H.initial xs

