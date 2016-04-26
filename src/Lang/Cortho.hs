{-# OPTIONS_HADDOCK hide, prune, ignore-exports, not-home #-}
{-|
Module      : Lang.Cortho
Description : Cortho API
Copyright   : (c) Benjamin F Jones, 2016
License     : BSD-3
Maintainer  : benjaminfjones@gmail.com
Stability   : experimental
Portability : POSIX

This module re-exports the primary modules making up Cortho for convenience.
-}

module Lang.Cortho
  ( -- * re-exports
    module X
  )
where

import Lang.Cortho.Parser  as X
import Lang.Cortho.Prelude as X
import Lang.Cortho.Types   as X
