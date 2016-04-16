{-
 - Parser.hs
 -
-}

module Lang.Cortho.Parser
  ( parseProgram
  )
where


import Text.Read.Lex
import Text.ParserCombinators.ReadP


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

parseProgram = error "not implemented"

num :: ReadP Integer
num = readDecP
