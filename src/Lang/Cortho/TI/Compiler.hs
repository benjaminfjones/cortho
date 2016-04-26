{-|
Module      : Lang.Cortho.TI.Compiler
Description : Template Instantiation Compiler for Cortho
Copyright   : (c) Benjamin F Jones, 2016
License     : BSD-3
Maintainer  : benjaminfjones@gmail.com
Stability   : experimental
Portability : POSIX

The TI compiler's job is to setup the initial state for performing template
instantiation (graph reduction). The main task is to setup the global
variables and initial heap/dump.

-}

module Lang.Cortho.TI.Compiler
  ( compile )
where

import           Lang.Cortho.Heap (Heap, Addr)
import qualified Lang.Cortho.Heap as H
import           Lang.Cortho.TI.Types


-- | The TI compiler pipeline
compile :: a
compile = error "TODO - compiler"
