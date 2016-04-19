{-
 - Parser.hs
 -
 - To test the parser, try the following:
 -
 -     ghci> import Text.ParserCombinators.ReadP
 -     ghci> let p = readP_to_S parseIdent
 -     ghci> p "alpha10 x = foo"
-}

module Lang.Cortho.Parser
  ( -- * core langauage syntax
    parseProgram
  , parseSC
  , parseExpr
  , parseIdent
  , parseNum
    -- * utilities
  , parseList
  , termList
  )
where


import Control.Monad
import Data.Char (isAlpha, isAlphaNum)
import Text.Read.Lex
import Text.ParserCombinators.ReadP

import Lang.Cortho.Types


------------------------------------------------------------------------
-- Keywords of the Core Language
------------------------------------------------------------------------

keywords :: [String]
keywords =
  [ "let"
  , "case"
  ]


------------------------------------------------------------------------
-- Parser Combinators for the Core Language
------------------------------------------------------------------------

{- Core Language Example:

main = double 2

double x = 2 * x
-}


-- | Parse a core language program
parseProgram :: ReadP Program
parseProgram = Program <$> termList parseSC

-- | Parse a core language supercombinator definition
parseSC :: ReadP ScDef
parseSC = do
  name  <- parseIdent
  binds <- parseList ws parseIdent
  _     <- char '=' >> ws
  rhs   <- parseExpr
  return $ ScDef { scName  = name
                 , scBinds = binds
                 , scExpr  = rhs
                 }

-- | Parse a core language expression
parseExpr :: ReadP CoreExpr
parseExpr = choice
    [ parseEVar
    , parseENum
    , parseEConstr
    , parseEAp
    , parseELet
    , parseECase
    , parseELam
    ]
  where
    parseEVar = EVar <$> parseIdent
    parseENum = ENum <$> parseNum

    parseEConstr = do
      string "Pack{" >> ws
      n <- parseNum
      char ',' >> ws
      k <- parseNum
      char '}' >> ws
      return $ EConstr n k

    parseEAp = do
      e1 <- parseExpr
      ws1
      e2 <- parseExpr
      return $ EAp e1 e2

    parseELet = do
      keyw <- string "letrec" <++ string "let"
      ws
      bind <- parseIdent
      char '=' >> ws
      e1 <- parseExpr
      string "in" >> ws
      e2 <- parseExpr
      return $ ELet (if keyw == "letrec" then True else False) (bind, e1) e2

    parseECase = do
      string "case" >> ws1
      e1 <- parseExpr
      ws1
      string "of" >> ws1
      term
      alts <- termList parseAlt
      return $ ECase e1 alts

    parseELam = do
      char '\\' >> ws
      vars <- parseList ws1 parseIdent
      string "->" >> ws
      e <- parseExpr
      return $ ELam vars e

parseAlt :: ReadP CoreAlter
parseAlt = undefined  -- TODO

-- | Non-alpha numeric characters allowed after the first character in an identifier
extraIdentChars :: [Char]
extraIdentChars = "_'"

-- | Parse an identifier consisting of an alpha character following by zero or
-- more AlphaNum characters, underscores, or primes (')
parseIdent :: ReadP Ident
parseIdent = do
    c:_   <- look
    first <- if isAlpha c then get
                          else pfail
    rest  <- munch isIdentChar
    ws  -- consume trailing whitespace
    return $ identFromStr (first : rest)
  where
    isIdentChar c = isAlphaNum c || c `elem` extraIdentChars

-- | Parse a signed integer and consume trailing whitespace
parseNum :: ReadP Int
parseNum = readDecP <* ws


-- Utilities -----------------------------------------------------------

-- | Parse a list of zero or more items separatd by elements parsed by 'psep'
parseList :: ReadP b -> ReadP a -> ReadP [a]
parseList = flip sepBy

-- | Parse a terminator separated list
termList :: ReadP a -> ReadP [a]
termList = parseList term

-- | Termination characters
termChars :: [Char]
termChars = ";\n"

-- | Terminator for supercombinator definitions
term :: ReadP ()
term = choice (map char termChars) >> skipSpaces

-- | Whitespace characters
wsChars :: [Char]
wsChars = " \t"

-- | Parse one whitespace character
wsChar :: ReadP Char
wsChar = choice (map char wsChars)

-- | Parse and throw away zero or more whitespace (spaces and tabs), but not newlines
ws :: ReadP ()
ws = look >>= skip
  where
    skip (c:s) | c `elem` wsChars = get >> skip s
    skip _                        = return ()

-- | Parse at least one whitespace character
ws1 :: ReadP ()
ws1 = wsChar >> ws
