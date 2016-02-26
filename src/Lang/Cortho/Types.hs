{-
- Lang.Cortho.Types
-
- This module contains all the core types for Cortho, in particular is defines
- and provides instances for the main expression type.
-}

module Lang.Cortho.Types
  ( -- * core expression type
    Expr(..)
  , Alter(..)
    -- * Auxilliary types
  , Ident
  )
where

import qualified Data.Text as T
import           Data.Text (Text)


-- | The core expression data type, parametrized over a base type
data Expr a
  = EVar !Ident        -- ^ variable identifier
  | ENum !Int          -- ^ number
  | EConstr            -- ^ data constructor
      !DataTag         --     data constructor tag
      !Int             --     airity
  | EAp                -- ^ function application
      !(Expr a)        --     function
      !(Expr a)        --     argument
  | ELet               -- ^ let/letrec expression
      !Bool            --     True <-> is "letrec", False <-> is "let"
      ![(Ident, Expr a)] -- bindings
      !(Expr a)        --     body
  | ECase              -- ^ case expression
      !(Expr a)        --     expression to case on
      ![Alter a]       --     case alternatives
  | ELam               -- ^ lambda
      ![Ident]         --     binders
      !(Expr a)        --     body
  deriving (Eq, Show)

-- | Case alternative, a pattern/expression pair
data Alter a
  = APattern           -- ^ non-trivial pattern match on a data constructor
      !DataTag         --     data constructor tag
      ![Ident]         --     binders
      !(Expr a)        --     right hand side
  | ADefault           -- ^ default "fall-through" case
      !(Expr a)        --     right hand side
  deriving (Eq, Show)

-- | Identifier used for names
type Ident = Text
-- | Data constructor tag
type DataTag = Int
