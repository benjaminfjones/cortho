{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Lang.Cortho.Prelude
Description : Cortho prelude
Copyright   : (c) Benjamin F Jones, 2016
License     : BSD-3
Maintainer  : benjaminfjones@gmail.com
Stability   : experimental
Portability : POSIX

A very minimal prelude for Cortho. We include the SKI combinators and some other
simple combinators for testing purposes.
-}

module Lang.Cortho.Prelude
  ( corePrelude
  , extraPrelude
  )
where

import Lang.Cortho.Types


-- | The core Cortho prelude.
corePrelude :: Program
corePrelude = Program $ map (\(x, y, z) -> ScDef x y z)
  [ ("I",  ["x"],                EVar "x")
  , ("K",  ["x", "y"],           EVar "x")
  , ("K1", ["x", "y"],           EVar "y")
  , ("S",  ["f", "g", "x"],      EAp (EAp (EVar "f") (EVar "x"))
                                     (EAp (EVar "g") (EVar "x")))
  , ("compose", ["f", "g", "x"], EAp (EVar "f")
                                     (EAp (EVar "g") (EVar "x")))
  , ("twice",   ["f"],           EAp (EAp (EVar "compose") (EVar "f"))
                                     (EVar "f"))
  ]

-- | Extra supercombinators not included in 'corePrelude'.
extraPrelude :: Program
extraPrelude = Program []
