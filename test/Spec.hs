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
pAlts    = parse parseAlts ""
pProgram = parse parseProgram ""

isLeft :: Either a b -> Bool
isLeft (Left _)  = True
isLeft (Right _) = False
isRight = not . isLeft

-- | Basic parser tests
parserTests = testGroup "Parser Units"
  [ testCase "parse ident" $
      pIdent "foo" @?= Right "foo"
  , testCase "parse ident fail" $
      assert $ isLeft (pIdent "123foo") 
  , testCase "parse num" $
      pNum "1024" @?= Right 1024
  , testCase "parse pattern" $
      pAlter "<0> x -> 1" @?= Right (APattern 0 ["x"] (ENum 1))
  , testCase "parse default" $
      pAlter "_ -> 1" @?= Right (ADefault (ENum 1))
  , testCase "parse alts" $
      pAlts  "<0> x -> 1; <1> x -> 2" @?=
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
    , ("multi ap", "f x y z",
        Right (EAp (EAp (EAp (EVar "f") (EVar "x")) (EVar "y")) (EVar "z")))
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
    , ("casepat", "case foo x of <0> x -> 1; <1> x -> 2",
        Right (ECase (EAp (EVar "foo") (EVar "x")) [ APattern 0 ["x"] (ENum 1)
                                                   , APattern 1 ["x"] (ENum 2)]))
    , ("caseboth", "case x of\n  <1> -> 5; _ -> 6\n",
        Right (ECase (EVar "x") [ APattern 1 [] (ENum 5)
                                , ADefault (ENum 6) ]))
    -- lambdas
    , ("lambda", "\\x -> x",
        Right (ELam ["x"] (EVar "x")))
    , ("lambda 2", "\\f x -> f x",
        Right (ELam ["f", "x"] (EAp (EVar "f") (EVar "x"))))
    , ("lambda case", "\\x -> case x of\n  <0> y -> 1; <1> z -> 2",
        Right (ELam ["x"] (ECase (EVar "x") [ APattern 0 ["y"] (ENum 1)
                                            , APattern 1 ["z"] (ENum 2)])))
    -- Arithmetic
    , ("arith +", "1 + 1",
        Right (EBinOp OpAdd (ENum 1) (ENum 1)))
    , ("arith -", "1 + (1 - 2)",
        Right (EBinOp OpAdd (ENum 1) (EBinOp OpSub (ENum 1) (ENum 2))))
    , ("arith *", "5*1 + (2*1 - 102)",
        Right (EBinOp OpAdd (EBinOp OpMult (ENum 5) (ENum 1))
                            (EBinOp OpSub (EBinOp OpMult (ENum 2) (ENum 1)) (ENum 102))))
    ]

nonExpressionParserTests = testGroup "Non-Expression Parser Units" $
  map (\(name, s, eval) -> testCase name (eval (pExpr s)))
    -- Note: parens around the expr's force the parse to parse it completely
    -- and not just succeed with the partial 1 - 1 or 1 / 1 prefix.
    [ ("non-assoc -", "(1 - 1 - 1)", assert . isLeft)
    , ("non-assoc /", "(1 / 1 / 1)", assert . isLeft)
    , ("non-assoc <", "(1 < 1 < 1)", assert . isLeft)
    , ("non-assoc >=", "(1 >= 1 <= 1)", assert . isLeft)
    ]

programParserTests = testGroup "Program Parser Units" $
  map (\(name, s, eval) -> testCase name (eval (pProgram s)))
    [ ("top level def", "main = 0",
        (@?= Right (Program [ScDef "main" [] (ENum 0)])))
    , ("main + function", "main = double x;\ndouble x = x x",
        (@?= Right (Program [ ScDef "main"   []    (EAp (EVar "double") (EVar "x"))
                            , ScDef "double" ["x"] (EAp (EVar "x") (EVar "x"))])))
    , ("ex1.21", "f = 3; \n\
                 \g x y = let z = x in z ;\n\
                 \h x = case (let y = x in y) of\n\
                 \        <1> -> 2;\n\
                 \        <2> -> 5",
        assert . isRight)
    , ("dangling else", "f x y = case x of\n\
                        \          <1> -> case y of\n\
                        \                   <1> -> 1;\n\
                        \          <2> -> 2",
      (@?= Right (let ap1 = APattern 1 [] (ENum 1) in
                  let ap2 = APattern 2 [] (ENum 2) in
                  let icase = ECase (EVar "y") [ap1, ap2] in
                  let ocase = ECase (EVar "x") [APattern 1 [] icase] in
                    Program [ScDef "f" ["x", "y"] ocase])))
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
          , nonExpressionParserTests
          , programParserTests
          ]

main :: IO ()
main = defaultMain suite
