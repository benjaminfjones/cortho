{-
- Lang.Cortho.TI.Types
-
- Type module for Cortho's template instantiation compiler / interpreter.
-
-}

module Lang.Cortho.TI.Types where


import Data.Map (Map)
import Lang.Cortho.Types (Ident)
import Lang.Cortho.Heap (Heap, Addr)


-- | Data type representing heap nodes. These correspond to either application
-- nodes in the AST, supercombinator applications, or constants (numbers).
data Node = NAp Addr Addr     -- ^ application node
          | NSC Ident [Addr]  -- ^ supercombinator node
          | NConst Integer    -- ^ constant node

-- | The spine of the current reduction, represented as a stack of heap
-- addresses.
type TIStack = [Addr]

-- | Dummy type for now
data TIDump = TIDummyDump

-- | Heap of nodes
type TIHeap = Heap Node

-- | Associate supercombinator names with their bdies
type TIGlobals = Map Ident Node

-- | TI interpreter state
type TIState = (TIStack, TIDump, TIHeap, TIGlobals)
