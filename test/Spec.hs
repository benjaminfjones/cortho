{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Text.Parsec

import Lang.Cortho


-- Parser Units --------------------------------------------------------

pIdent   = parse parseIdent ""
pExpr    = parse parseExpr ""
pNum     = parse parseNum ""
pAlter   = parse parseAlt ""
pAlts    = parse (termList1 parseAlt) ""
pProgram = parse parseProgram ""

isLeft :: Either a b -> Bool
isLeft (Left _)  = True
isLeft (Right _) = False

-- | Basic parser tests
parserTests = testGroup "Parser Units"
  [ testCase "parse ident" $
      pIdent "foo" @?= Right "foo"
  , testCase "parse ident fail" $
      assert $ isLeft (pIdent "123foo") 
  , testCase "parse num" $
      pNum "1024" @?= Right 1024
  , testCase "parse pattern" $
      pAlter "<0> x -> 1;" @?= Right (APattern 0 ["x"] (ENum 1))
  , testCase "parse default" $
      pAlter "_ -> 1;" @?= Right (ADefault (ENum 1))
  , testCase "parse alts" $
      pAlts  "<0> x -> 1; <1> x -> 2;" @?=
      Right [APattern 0 ["x"] (ENum 1), APattern 1 ["x"] (ENum 2)]
  ]

-- | Expression parser tests
exprParserTests = testGroup "Expr Parser Units" $
  map (\(name, s, res) -> testCase name (pExpr s @?= res))
    [ ("var", "foo",
        Right (EVar "foo"))
    , ("num", "2048",
        Right (ENum 2048))
    -- constructors
    , ("pack",  "Pack{1,1}",
        Right (EConstr 1 1))
    , ("pack ws",  "Pack{ 1, 1 }",
        Right (EConstr 1 1))
    -- apps
    , ("ap",  "foo bar",
        Right (EAp (EVar "foo") (EVar "bar")))
    , ("ap2", "f (g x)",
        Right (EAp (EVar "f") (EAp (EVar "g") (EVar "x"))))
    , ("ap3", "f ((h g) x)",
        Right (EAp (EVar "f") (EAp (EAp (EVar "h") (EVar "g")) (EVar "x"))))
    , ("non-ap", "foo of", Right (EVar "foo"))
    -- lets
    , ("let",  "let x=5 in x",
        Right (ELet False [("x", ENum 5)] (EVar "x")))
    , ("let2", "let x=5; y=6 in x",
        Right (ELet False [("x", ENum 5), ("y", ENum 6)] (EVar "x")))
    , ("letrec",  "letrec x=y; z=w in x",
        Right (ELet True [ ("x", EVar "y"), ("z", EVar "w")] (EVar "x")))
    , ("letrec2", "letrec x=y in x",
        Right (ELet True [("x", EVar "y")] (EVar "x")))
    -- cases
    , ("casepat", "case foo x of <0> x -> 1; <1> x -> 2;",
        Right (ECase (EAp (EVar "foo") (EVar "x")) [ APattern 0 ["x"] (ENum 1)
                                                   , APattern 1 ["x"] (ENum 2)]))
    , ("caseboth", "case x of\n  <1> -> 5; _ -> 6;\n",
        Right (ECase (EVar "x") [ APattern 1 [] (ENum 5)
                                , ADefault (ENum 6) ]))
    -- lambdas
    , ("lambda", "\\x -> x",
        Right (ELam ["x"] (EVar "x")))
    , ("lambda 2", "\\f x -> f x",
        Right (ELam ["f", "x"] (EAp (EVar "f") (EVar "x"))))
    , ("lambda case", "\\x -> case x of\n  <0> y -> 1; <1> z -> 2;",
        Right (ELam ["x"] (ECase (EVar "x") [ APattern 0 ["y"] (ENum 1)
                                            , APattern 1 ["z"] (ENum 2)])))
    ]

programParserTests = testGroup "Program Parser Units" $
  map (\(name, s, res) -> testCase name (pProgram s @?= res))
    [ ("top level def", "main = 0;",
        Right (Program [ScDef "main" [] (ENum 0)]))
    , ("main + function", "main = double x;\ndouble x = x x;",
        Right (Program [ ScDef "main"   []    (EAp (EVar "double") (EVar "x"))
                       , ScDef "double" ["x"] (EAp (EVar "x") (EVar "x"))]))
    ]

-- Properties ----------------------------------------------------------

--    testGroup "Trivial QuickCheck tests"
--      [ testProperty "Quickcheck test" arith
--      , testProperty "Negation" negation
--      ]


-- Main -----------------------------------------------------------------

suite :: TestTree
suite = testGroup "Test Suite"
          [ parserTests
          , exprParserTests
          , programParserTests
          ]

main :: IO ()
main = defaultMain suite
