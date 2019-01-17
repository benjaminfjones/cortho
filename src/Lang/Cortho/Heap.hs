{-|
Module      : Lang.Cortho.Heap
Description : Heap implementation
Copyright   : (c) Benjamin F Jones, 2016
License     : BSD-3
Maintainer  : benjaminfjones@gmail.com
Stability   : experimental
Portability : POSIX

A simple heap (in the memory sense, not the data structure sense) implementation
based on Haskell standard lib containers. This module is meant to be imported
qualified.
-}

module Lang.Cortho.Heap
  ( -- * Heap API
    Heap
  , initial
  , alloc
  , alloc'
  , free
  , size
  , lookup
  , update
  , addresses
    -- * Heap address type
  , Addr
  )
where

import           Prelude hiding (lookup)

import           Data.Map (Map)
import qualified Data.Map as M


-- Heap ----------------------------------------------------------------

-- | A heap type implemented using "Data.Map".
data Heap a = Heap
  { hSize   :: !Int        -- ^ heap size
  , hUnused :: [Addr]      -- ^ unused addresses
  , hAssoc  :: Map Addr a  -- ^ addr -> object association
  }

instance Show (Heap a) where
  show h = "Heap of size " ++ show (hSize h)

-- | An initial, empty heap.
initial :: Heap a
initial =
  Heap { hSize = 0
       , hUnused = map Addr [0..]
       , hAssoc = M.empty
       }

-- | Return the number of objects in the heap.
size :: Heap a -> Int
size = hSize

-- | Allocate the given object on the heap.
alloc :: Heap a -> a -> (Heap a, Addr)
alloc h obj = (heap', addr)
  where
    addr:rest = hUnused h
    heap' = Heap (hSize h + 1) rest (M.insert addr obj (hAssoc h))

-- | Version of 'alloc' that does not return the allocation address.
alloc' :: Heap a -> a -> Heap a
alloc' h o = fst (alloc h o)

-- | Remove the object at the given addresss from the heap. If the address
-- is unallocated, return the original heap. 'free' does not change the unused
-- addresses.
free :: Heap a -> Addr -> Heap a
free h addr =
  case lookup h addr of
    Nothing -> h
    Just _  -> h { hSize = hSize h - 1
                 , hAssoc = M.delete addr (hAssoc h)
                 }

-- | Lookup the object at a given address in the heap.
lookup :: Heap a -> Addr -> Maybe a
lookup h addr = M.lookup addr (hAssoc h)

-- | Replace the object at the given address with a new one. If the address
-- is not occupied, return Nothing.
update :: Heap a -> Addr -> a -> Maybe (Heap a)
update h addr obj =
  case lookup h addr of
    Nothing -> Nothing
    Just _  -> Just $ h { hAssoc = M.update (\_ -> Just obj) addr (hAssoc h) }

-- | Return all allocated addresses.
addresses :: Heap a -> [Addr]
addresses h = M.keys (hAssoc h)


-- Addr ----------------------------------------------------------------

-- | 'Addr' is a unique address for a heap object. It is kept abstract
-- for safety.
newtype Addr = Addr { addrRep :: Int }
  deriving (Eq, Ord)

instance Show Addr where
  show = show . addrRep
