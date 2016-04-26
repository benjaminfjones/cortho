{-
- Compiler.hs
-
- Template Instantiation Compiler for Cortho.
-
- The TI compiler's job is to setup the initial state for performing template
- instantiation (graph reduction). The main task is to setup the global
- variables and initial heap/dump.
-
-}

module Lang.Cortho.TI.Compiler
  ( compile )
where


import           Lang.Cortho.Heap (Heap, Addr)
import qualified Lang.Cortho.Heap as H
import           Lang.Cortho.TI.Types


compile = error "TODO - compiler"
