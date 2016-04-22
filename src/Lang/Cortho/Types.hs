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


import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Exts (IsString(..))
import           Text.PrettyPrint.HughesPJClass


------------------------------------------------------------------------
-- Core Types
------------------------------------------------------------------------

-- | Identifier used for names
newtype Ident = Ident { unIdent :: Text }
  deriving (Eq, Show)

identFromText = Ident
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
  = EVar !Ident        -- ^ variable identifier
  | ENum  Integer      -- ^ number
  | EBinOp             -- ^ infix binary operator
       BinOp           --     operator
      !(Expr a)        --     left argument
      !(Expr a)        --     right argument
  | EUnaOp             -- ^ prefix unary operator
       UnaryOp         --     unary operator
      !(Expr a)        --     unary argument
  | EConstr            -- ^ data constructor
       Int             --     data constructor tag
       Int             --     airity
  | EAp                -- ^ function application
      !(Expr a)        --     function
      !(Expr a)        --     argument
  | ELet               -- ^ let/letrec expression
       Bool            --     True <-> is "letrec", False <-> is "let"
      ![(a, Expr a)]   --     a list of bindings
      !(Expr a)        --     body
  | ECase              -- ^ case expression
      !(Expr a)        --     expression to case on
      ![Alter a]       --     case alternatives
  | ELam               -- ^ lambda
      ![a]         --     binders
      !(Expr a)        --     body
  deriving (Eq, Show)

isAtom :: Expr a -> Bool
isAtom (EVar _)      = True
isAtom (ENum _)      = True
isAtom (EConstr _ _) = True
isAtom (EAp _ _)     = False
isAtom (ELet _ _ _)  = False
isAtom (ECase _ _)   = False
isAtom (ELam _ _)    = False

-- | CoreExpr is the usual expression type where binders are just names
type CoreExpr = Expr Ident
type CoreAlter = Alter Ident

-- | Case alternative, a pattern/expression pair
data Alter a
  = APattern           -- ^ non-trivial pattern match on a data constructor
      !Int             --     data constructor tag
      ![a]             --     binders
      !(Expr a)        --     right hand side
  | ADefault           -- ^ default "fall-through" case
      !(Expr a)        --     right hand side
  deriving (Eq, Show)

-- | Binary Operators. Precedence is encoded in the parser.
data BinOp
  -- * Arithmetic Operators.
  = OpAdd    -- ^ Addition (+)
  | OpSub    -- ^ Subtraction (-)
  | OpMult   -- ^ Multiplication (*)
  | OpDiv    -- ^ Division (/)
  -- * Relational operators. Precedence is encoded in the parser.
  | OpLT     -- ^ Less than (<)
  | OpLE     -- ^ Less or equal (<=)
  | OpGT     -- ^ Greater than (>)
  | OpGE     -- ^ Greater or equal (>=)
  | OpEQ     -- ^ Equal (==)
  | OpNEQ    -- ^ Not Equal (/=)
-- * Boolean Operators.
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

pBind :: Pretty a => (a, Expr a) -> Doc
pBind (name, expr) = pPrint name <> equals <> pPrint expr <> text ";"


