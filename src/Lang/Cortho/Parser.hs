{-# LANGUAGE OverloadedStrings #-}

{-
 - Parser.hs
 -
 - To test the parser, try the following:
 -
 -     ghci> import Text.ParserCombinators.Parser
 -     ghci> let p = readP_to_S parseIdent
 -     ghci> p "alpha10 x = foo"
-}

module Lang.Cortho.Parser
  ( -- * core langauage syntax
    parseProgram
  , parseSC
  , parseExpr
  , parseAlt
  , parseIdent
  , parseNum
    -- * utilities
  , parseList
  , termList
  , termList1
  )
where


import Control.Monad
import Data.Char (isAlphaNum)
import Data.Foldable (foldl')
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Exts (IsString(..))
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.PrettyPrint.HughesPJClass (pPrint)

import Lang.Cortho.Types

import Debug.Trace (trace)

------------------------------------------------------------------------
-- Keywords of the Core Language
------------------------------------------------------------------------

keywords :: [Text]
keywords = map T.pack
  [ "let"
  , "letrec"
  , "in"
  , "case"
  , "of"
  ]

isKeyword :: Ident -> Bool
isKeyword s = unIdent s `elem` keywords


------------------------------------------------------------------------
-- Parser Combinators for the Core Language
------------------------------------------------------------------------

{- Core Language Example:

> main = double 2;
> double x = 2 * x;

-}


-- | Parse a core language program
parseProgram :: Parser Program
parseProgram = Program <$> termList parseSC

-- | Parse a core language supercombinator definition
parseSC :: Parser ScDef
parseSC = do
  name  <- parseIdent
  binds <- parseIdent `sepBy` ws
  equals
  rhs <- parseExpr
  -- termination handled by termList
  return $ ScDef
             { scName  = name
             , scBinds = binds
             , scExpr  = rhs
             }

-- | Parse a core language expression
--
-- Try to parse a function application first (since the first part of that
-- parse overlaps with 'parseExprLhs'. If that parse fails, then parse a
-- non-application exprssion.
parseExpr :: Parser CoreExpr
parseExpr = try parseEAp <|> parseExprLhs
  where
    -- Parse all expression forms except for application
    parseExprLhs =
      parseEConstr   <|>
      parseELet      <|>
      parseECase     <|>
      parseELam      <|>
      parseParenExpr <|>
      parseENum      <|>
      parseEVar

    -- Parse identifier expressions (but not keywords)
    parseEVar = do
      s <- parseIdent
      if isKeyword s
         then mzero  -- fail
         else return $ EVar s
    parseENum = ENum <$> parseNum

    -- Data constructor: note no whitespace in pack for now
    parseEConstr = do
      kw "Pack"
      symbol '{'
      n <- parseNum
      symbol ','
      k <- parseNum
      symbol '}'
      return $ EConstr n k

    -- Application
    parseEAp = do
      e1 <- parseExprLhs
      e2 <- parseExpr
      return $ EAp e1 e2

    -- Let/Letrec expression
    parseELet = do
      --keyw <- try (lexeme (string "letrec")) <|> lexeme (string "let")
      void $ string "let"
      mrec <- optionMaybe $ try (string "rec")
      ws1
      decls <- parseList term parseDecl
      kw "in"
      e2 <- parseExpr
      return $ ELet (isJust mrec) decls e2

    parseDecl = do
      EVar bind <- parseEVar
      symbol '='
      e1 <- parseExpr
      return (bind, e1)

    -- Case expression
    parseECase = do
      kw "case"
      e1 <- parseExpr
      kw "of"
      alts <- termList1 parseAlt
      return $ ECase e1 alts

    -- Lambda
    parseELam = do
      lambda
      vars <- parseIdent `sepBy` ws
      arrow
      e <- parseExpr
      return $ ELam vars e

    -- Expr in parens
    parseParenExpr = between (symbol '(') (symbol ')') parseExpr <* ws

-- | Parse a case alternative
parseAlt :: Parser CoreAlter
parseAlt = (try parseAPattern <?> "error at: parsePattern") <|> parseADefault
  where
    parseAPattern = do
      tag <- between (symbol '<') (symbol '>') parseNum
      binds <- parseIdent `sepBy` ws
      arrow
      expr <- try parseExpr
      return $ APattern tag binds expr
    parseADefault = do
      symbol '_'
      arrow
      expr <- parseExpr
      -- term separation is handled by termList(1)
      return $ ADefault expr

-- | Non-alpha numeric characters allowed after the first character in an identifier
extraIdentChars :: [Char]
extraIdentChars = "_'"

-- | Parse an identifier consisting of an alpha character following by zero or
-- more AlphaNum characters, underscores, or primes (')
parseIdent :: Parser Ident
parseIdent = do
  c  <- letter
  cs <- many (satisfy nonFirstChar)
  ws
  return $ identFromStr (c:cs)
  where
    nonFirstChar c = isAlphaNum c || c `elem` extraIdentChars

-- | Parse an unsigned integer
parseNum :: (Num a, Read a) => Parser a
parseNum = do
  digits <- lexeme (many1 digit)
  return (read digits)


-- Utilities -----------------------------------------------------------

-- | Use the given parser and then consume any trailing whitespace
lexeme :: Parser a -> Parser a
lexeme p = p <* ws

-- | Parse the given keyword as a lexeme
kw :: String -> Parser ()
kw = void . lexeme . string

-- | Parse a list of zero or more items (2nd parser) separatd by elements (1st
-- parser)
parseList :: Parser b -> Parser a -> Parser [a]
parseList = flip sepBy

-- | Parse a sequence of terminated 'p's
termList :: Parser a -> Parser [a]
termList p = endBy p term

-- | Parse a non-empty sequence of terminated 'p's
termList1 :: Parser a -> Parser [a]
termList1 p = endBy1 p term

-- | Terminator characters
termChar :: Char
termChar = ';'

-- | Terminator for supercombinator definitions, let bindings, and case
-- alternatives
term :: Parser ()
term = void $ lexeme (char termChar)

-- | Whitespace characters
wsChars :: [Char]
wsChars = " \t\n"

-- | Parse one whitespace character
wsChar :: Parser Char
wsChar = satisfy (`elem` wsChars)

-- | Parse and throw away zero or more whitespace (spaces and tabs), but not newlines
ws :: Parser ()
ws = void $ many wsChar

-- | Parse at least one whitespace character
ws1 :: Parser ()
ws1 = void $ many1 wsChar


-- Various symbol lexemes ----------------------------------------------

symbol c = lexeme (char c)
arrow    = lexeme (string "->")
lambda   = lexeme (char '\\')
equals   = lexeme (char '=')
