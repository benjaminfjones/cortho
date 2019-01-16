{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Lang.Cortho.Types
Description : Core Cortho types
Copyright   : (c) Benjamin F Jones, 2016
License     : BSD-3
Maintainer  : benjaminfjones@gmail.com
Stability   : experimental
Portability : POSIX

This module contains all the core types for Cortho. In particular it defines
and provides instances for the main expression type.
-}

module Lang.Cortho.Types
  ( -- * core expression type
    CoreExpr
  , CoreAlter
  , Expr(..)
  , Alter(..)
  , Program(..)
  , ScDef(..)
    -- * Auxilliary types
  , BinOp(..)
  , UnaryOp(..)
  , Ident
  , unIdent
  , identFromText
  , identFromStr
  )
where


import           Prelude hiding ((<>))

import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Exts (IsString(..))
import           Text.PrettyPrint.HughesPJClass


------------------------------------------------------------------------
-- Core Types
------------------------------------------------------------------------

-- | Identifier used for names
newtype Ident = Ident { unIdent :: Text {-^ unwrap an 'Ident' -} }
  deriving (Eq, Show)

-- | Text -> Ident
identFromText = Ident
-- | String -> Ident
identFromStr  = Ident . T.pack

instance IsString Ident where
  fromString = Ident . T.pack

-- | Core program type, a list of supercombinator definitions
data Program = Program [ScDef]
  deriving (Eq, Show)

-- | Supercombinator definition
data ScDef = ScDef
  { scName  :: Ident     -- ^ name
  , scBinds :: [Ident]   -- ^ binders (arguments)
  , scExpr  :: CoreExpr  -- ^ right-hand side
  }
  deriving (Eq, Show)

-- | The core expression data type, parametrized over a binders type
data Expr a
  = EVar !Ident                        -- ^ variable identifier
  | ENum  Integer                      -- ^ number
  | EBinOp BinOp !(Expr a) !(Expr a)   -- ^ binary operator
  | EUnaOp UnaryOp !(Expr a)           -- ^ prefix unary operator
  | EConstr Int Int                    -- ^ data constructor
  | EAp !(Expr a) !(Expr a)            -- ^ function application
  | ELet Bool ![(a, Expr a)] !(Expr a) -- ^ let/letrec expression (True <-> letrec)
  | ECase !(Expr a) ![Alter a]         -- ^ case expression
  | ELam ![a] !(Expr a)                -- ^ lambda
  deriving (Eq, Show)

-- | Classify expressions as atoms and non-atoms
isAtom :: Expr a -> Bool
isAtom (EVar _)      = True
isAtom (ENum _)      = True
isAtom (EConstr _ _) = True
isAtom (EAp _ _)     = False
isAtom (ELet _ _ _)  = False
isAtom (ECase _ _)   = False
isAtom (ELam _ _)    = False

-- | CoreExpr is the usual expression type where binders are names
type CoreExpr = Expr Ident
-- | CoreAlter is the case alternative type where binders are names
type CoreAlter = Alter Ident

-- | Case alternative, a pattern/expression pair
data Alter a
  = APattern !Int ![a] !(Expr a) -- ^ non-trivial data c'tor pattern match
  | ADefault !(Expr a)           -- ^ default "fall-through" case
  deriving (Eq, Show)

-- | Binary Operators. Precedence is encoded in the parser.
data BinOp
  = -- Arithmetic Operators
    OpAdd    -- ^ Addition (+)
  | OpSub    -- ^ Subtraction (-)
  | OpMult   -- ^ Multiplication (*)
  | OpDiv    -- ^ Division (/)
    -- Relational operators. Precedence is encoded in the parser.
  | OpLT     -- ^ Less than (<)
  | OpLE     -- ^ Less or equal (<=)
  | OpGT     -- ^ Greater than (>)
  | OpGE     -- ^ Greater or equal (>=)
  | OpEQ     -- ^ Equal (==)
  | OpNEQ    -- ^ Not Equal (/=)
    -- Boolean Operators.
  | OpAnd    -- ^ Boolean and (&)
  | OpOr     -- ^ Boolean or  (|)
  deriving (Eq, Show)

-- | Unary Operators. Note: unary negation is handled by the 'negate'
-- prelude function.
data UnaryOp
  = OpNot    -- ^ Boolean negation (~)
  deriving (Eq, Show)

------------------------------------------------------------------------
-- Pretty Printing
------------------------------------------------------------------------

-- | Number of spaces to indent
ispace :: Int
ispace = 2

instance Pretty Ident where
  pPrint = text . T.unpack . unIdent

instance Pretty ScDef where
  pPrint (ScDef name vars expr) =
    pPrint name <+> hsep (pPrint <$> vars) <+> equals <+> pPrint expr

instance Pretty Program where
  pPrint (Program sds) = vcat (pPrint <$> sds)

instance Pretty a => Pretty (Expr a) where
  pPrint (EVar x) = pPrint x
  pPrint (ENum x) = pPrint x
  pPrint (EBinOp op x y) = pPrint x <> pPrint op <> pPrint y
  pPrint (EUnaOp op x) = pPrint op <> pPrint x
  pPrint (EConstr t a) = text "Pack{" <> int t <> text "," <> int a <> text "}"
  pPrint (EAp f a) | isAtom a  = pPrint f <+> pPrint a
                   | otherwise = pPrint f <+> parens (pPrint a)
  pPrint (ELet b decls body) =
    text (if b then "letrec" else "let") <+> hsep (map pBind decls) <+> text "in" <+>
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

instance Pretty BinOp where
  pPrint OpAdd   = text " + "
  pPrint OpSub   = text " - "
  pPrint OpMult  = text "*"
  pPrint OpDiv   = text "/"
  pPrint OpLT    = text " < "
  pPrint OpLE    = text " <= "
  pPrint OpGT    = text " > "
  pPrint OpGE    = text " >= "
  pPrint OpEQ    = text " == "
  pPrint OpNEQ   = text " != "
  pPrint OpAnd   = text " & "
  pPrint OpOr    = text " | "

instance Pretty UnaryOp where
  pPrint OpNot   = text "~"

-- | Pretty print a variable binding
pBind :: Pretty a => (a, Expr a) -> Doc
pBind (name, expr) = pPrint name <> equals <> pPrint expr <> text ";"
