{-# LANGUAGE OverloadedStrings #-}

{-
- Lang.Cortho.Types
-
- This module contains all the core types for Cortho, in particular is defines
- and provides instances for the main expression type.
-}

module Lang.Cortho.Types
  ( -- * core expression type
    CoreExpr
  , Expr(..)
  , Alter(..)
  , Program
  , ScDef(..)
    -- * Auxilliary types
  , Ident
  )
where

import qualified Data.Text as T
import           Data.Text (Text)
import           GHC.Exts (IsString(..))
import           Text.PrettyPrint.HughesPJClass


-- | Identifier used for names
newtype Ident = Ident { unIdent :: Text }

instance IsString Ident where
  fromString = Ident . T.pack

------------------------------------------------------------------------
-- Core Types
------------------------------------------------------------------------

-- | Core program type, a list of supercombinator definitions
type Program = [ScDef]

-- | Supercombinator definition
data ScDef = ScDef
  { scName  :: Ident     -- ^ name
  , scBinds :: [Ident]   -- ^ binders (arguments)
  , scExpr  :: CoreExpr  -- ^ right-hand side
  }

-- | The core expression data type, parametrized over a binders type
data Expr a
  = EVar !Ident        -- ^ variable identifier
  | ENum !Int          -- ^ number
  | EConstr            -- ^ data constructor
      !Int             --     data constructor tag
      !Int             --     airity
  | EAp                -- ^ function application
      !(Expr a)        --     function
      !(Expr a)        --     argument
  | ELet               -- ^ let/letrec expression
      !Bool            --     True <-> is "letrec", False <-> is "let"
      !(a, Expr a)     --     a binding
      !(Expr a)        --     body
  | ECase              -- ^ case expression
      !(Expr a)        --     expression to case on
      ![Alter a]       --     case alternatives
  | ELam               -- ^ lambda
      ![a]         --     binders
      !(Expr a)        --     body

-- | CoreExpr is the usual expression type where binders are just names
type CoreExpr = Expr Ident

-- | Case alternative, a pattern/expression pair
data Alter a
  = APattern           -- ^ non-trivial pattern match on a data constructor
      !Int             --     data constructor tag
      ![a]             --     binders
      !(Expr a)        --     right hand side
  | ADefault           -- ^ default "fall-through" case
      !(Expr a)        --     right hand side


------------------------------------------------------------------------
-- Pretty Printing
------------------------------------------------------------------------

-- | Number of spaces to indent
ispace :: Int
ispace = 2

instance Pretty Ident where
  pPrint = text . T.unpack . unIdent

instance Pretty a => Pretty (Expr a) where
  pPrint (EVar x) = pPrint x
  pPrint (ENum x) = pPrint x
  pPrint (EConstr t a) = text "Pack{" <> int t <> text "," <> int a <> text "}"
  pPrint (EAp f a) = pPrint f <+> pPrint a
  pPrint (ELet b binds body) =
    text (if b then "letrec" else "let") <+> pBinds binds <+> text "in" <+>
         pPrint body
  pPrint (ECase c as) =
    text "case" <+> pPrint c <+> text "of" $$
         nest ispace (vcat . map pPrint $ as)
  pPrint (ELam vs e) = parens (text "\\" <> pPrint vs <> text "->" <+> pPrint e)

instance Pretty a => Pretty (Alter a) where
  pPrint (APattern t vs e) =
    text "<" <> int t <> text ">" <+> hsep (map pPrint vs) <+> text "->" <+>
    pPrint e
  pPrint (ADefault e) =
    text "_" <+> text "->" <+> pPrint e

pBinds :: Pretty a => (a, Expr a) -> Doc
pBinds (name, expr) = pPrint name <+> equals <+> pPrint expr


