{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Lang.Cortho.Parser
Description : Cortho parser
Copyright   : (c) Benjamin F Jones, 2016
License     : BSD-3
Maintainer  : benjaminfjones@gmail.com
Stability   : experimental
Portability : POSIX

This module implements the parser for Cortho using parsec ("Text.Parsec"). It
handles precedence and associativity of all the binary / unary operators and
funciton application.

-}

module Lang.Cortho.Parser
  ( -- * core langauage syntax
    parseProgram
  , parseSC
  , parseExpr
  , parseAlt
  , parseAlts
  , parseIdent
  , parseNum
    -- * utilities
  , parseList
    -- * re-exported from Text.Parsec
  , parse
  )
where


import Control.Monad
import Data.Char (isAlphaNum)
import Data.Foldable (foldl1)
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
keywords =
  [ "let"
  , "letrec"
  , "in"
  , "case"
  , "of"
  , "Pack"
  ]

isKeyword :: Ident -> Bool
isKeyword s = unIdent s `elem` keywords


------------------------------------------------------------------------
-- Parser Combinators for the Core Language
------------------------------------------------------------------------

{- Core Language Example:

> main = double 2;
> double x = 2 * x

> f = 3;
> g x y = let z = x in z;
> h x = case (let y = x in y) of
>         <1> -> 2;
>         <2> -> 5

-}


-- | Parse a core language program
parseProgram :: Parser Program
parseProgram = Program <$> (parseSC `sepBy` term)

-- | Parse a core language supercombinator definition
parseSC :: Parser ScDef
parseSC = do
  name  <- parseIdent
  binds <- parseIdent `sepBy` ws
  equals
  rhs <- parseExpr
  return $ ScDef
             { scName  = name
             , scBinds = binds
             , scExpr  = rhs
             }

-- | Parse a core language expression
parseExpr :: Parser CoreExpr
parseExpr =  parseELet
         <|> parseECase
         <|> parseELam
         <|> parseE1
  where
    -- Let/Letrec expression
    parseELet = do
      void $ string "let"
      mrec <- optionMaybe $ try (string "rec")
      ws1
      decls <- parseDecl `sepBy1` term
      kw "in"
      e2 <- parseExpr
      return $ ELet (isJust mrec) decls e2

    -- Let variable binding
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
      alts <- parseAlts
      return $ ECase e1 alts

    -- Lambda
    parseELam = do
      lambda
      vars <- parseIdent `sepBy` ws
      arrow
      e <- parseExpr
      return $ ELam vars e

    -- The following productions encode operator precedence ----------------

    -- Case: expr1 -> expr2 | expr1. (|) is right associative.
    parseE1 = do
      e2  <- parseE2
      e1' <- parseE1'
      return $ e1' e2

    -- | Return a CoreExpr builder in the 'expr1' case
    parseE1' :: Parser (CoreExpr -> CoreExpr)
    parseE1' = parseOrRHS <|> (ws >> return id)
      where
        parseOrRHS = do
          symbol '|'
          e1 <- parseE1
          return (\e2 -> EBinOp OpOr e2 e1)

    -- Case: expr2 -> expr3 & expr2. (&) is right associative.
    parseE2 = do
      e3  <- parseE3
      e2' <- parseE2'
      return $ e2' e3

    parseE2' = parseAndRHS <|> (ws >> return id)
      where
        parseAndRHS = do
          symbol '&'
          e2 <- parseE2
          return (\e3 -> EBinOp OpAnd e3 e2)

    -- Case: expr3 -> expr4 relop expr4. Relational operators are non-associative.
    parseE3 = do
      e4  <- parseE4
      e3' <- parseE3'
      return $ e3' e4

    parseE3' = parseRelOpRHS <|> (ws >> return id)
      where
        parseRelOpRHS = do
          op <- relop
          e4' <- parseE4
          return (\e4 -> EBinOp op e4 e4')

    -- Case: expr4 -> expr5 +- expr4
    parseE4 = do
      e5 <- parseE5
      f  <- parseE4'
      return $ f e5

    parseE4' = parseAddOpRHS <|> parseSubOpRHS <|> (ws >> return id)
      where
        parseAddOpRHS = do  -- + is right associative
          symbol '+'
          e4' <- parseE4
          return (\e5 -> EBinOp OpAdd e5 e4')
        parseSubOpRHS = do  -- - is non-associative
          symbol '-'
          e5' <- parseE5
          return (\e5 -> EBinOp OpSub e5 e5')

    -- Case: expr5 -> expr6 */ expr5(6)
    parseE5 = do
      e6 <- parseE6
      f  <- parseE5'
      return $ f e6

    parseE5' = parseMultOpRHS <|> parseDivOpRHS <|> (ws >> return id)
      where
        parseMultOpRHS = do  -- note: * is right associative
          symbol '*'
          e5' <- parseE5
          return (\e6 -> EBinOp OpMult e6 e5')
        parseDivOpRHS = do  -- note: / is non-associative
          symbol '/'
          e6' <- parseE6
          return (\e6 -> EBinOp OpDiv e6 e6')

    -- Highest precedence sort of expr are the atomic ones and function
    -- applications (left associative).
    parseE6 = do
      exprs <- parseAtomic `sepBy1` ws
      case exprs of
        []  -> error "fatal parser error: sepBy1 returned nothing"
        [e] -> return $ e                 -- atomic expr
        _   -> return $ foldl1 EAp exprs  -- application

    -- Atomic expressions
    parseAtomic =  parseEVar
               <|> parseENum
               <|> parseEConstr
               <|> parseParenExpr

    -- Identifier expressions (but not keywords)
    parseEVar = try $ do
      s <- parseIdent
      if isKeyword s
         then mzero  -- fail
         else return $ EVar s

    -- Integers
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
      return $ ADefault expr

-- | Parse one of more case alternatives separated by terminators
parseAlts :: Parser [CoreAlter]
parseAlts = parseAlt `sepBy1` term

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

-- | Parse and throw away zero or more whitespace (spaces and tabs), but not
-- newlines
ws :: Parser ()
ws = void $ many wsChar

-- | Parse at least one whitespace character
ws1 :: Parser ()
ws1 = void $ many1 wsChar


-- Various symbol lexemes ----------------------------------------------

symbol = lexeme . char
bigSym = lexeme . string
arrow  = lexeme (string "->")
lambda = lexeme (char '\\')
equals = lexeme (char '=')

arithOp :: Parser BinOp
arithOp =  (symbol '+' >> return OpAdd)
       <|> (symbol '-' >> return OpSub)
       <|> (symbol '*' >> return OpMult)
       <|> (symbol '/' >> return OpDiv)

relop :: Parser BinOp
relop =  (bigSym "<=" >> return OpLE)
     <|> (bigSym "<"  >> return OpLT)
     <|> (bigSym ">=" >> return OpGE)
     <|> (bigSym ">"  >> return OpGT)
     <|> (bigSym "==" >> return OpEQ)
     <|> (bigSym "!=" >> return OpNEQ)
