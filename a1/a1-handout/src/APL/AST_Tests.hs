module APL.AST_Tests (tests) where

import APL.AST (Exp (..), printExp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Prettyprinting"
    [ testCase "CstInt" $
        printExp (CstInt 42) @?= "42",
      testCase "CstBool" $
        printExp (CstBool True) @?= "true",
      testCase "Add" $
        printExp (Add (CstInt 2) (CstInt 3)) @?= "(2 + 3)",
      testCase "If" $
        printExp (If (CstBool True) (CstInt 1) (CstInt 2))
          @?= "(if true then 1 else 2)",
      testCase "Let" $
        printExp (Let "x" (CstInt 5) (Var "x"))
          @?= "(let x = 5 in x)",
      testCase "ForLoop" $
        printExp (ForLoop ("p", CstInt 0) ("i", CstInt 10) (Add (Var "p") (Var "i")))
          @?= "(loop p = 0 for i < 10 do (p + i))",
      testCase "Lambda" $
        printExp (Lambda "x" (Add (Var "x") (CstInt 1)))
          @?= "(\\x -> (x + 1))",
      testCase "Apply" $
        printExp (Apply (Var "f") (CstInt 2))
          @?= "(f 2)",
      testCase "TryCatch" $
        printExp (TryCatch (CstInt 1) (CstInt 2))
          @?= "(try 1 catch 2)",
      testCase "Lambda" $
        printExp (Lambda "x" (Add (Var "x") (CstInt 1)))
          @?= "(\\x -> (x + 1))",
      testCase "Nested arithmetic" $
        printExp (Mul (Add (CstInt 1) (CstInt 2)) (CstInt 3))
          @?= "((1 + 2) * 3)",

      testCase "Nested lambda apply" $
        printExp (Apply (Lambda "x" (Add (Var "x") (CstInt 1))) (CstInt 5))
          @?= "((\\x -> (x + 1)) 5)",

      testCase "Nested TryCatch" $
        printExp (TryCatch (Div (CstInt 1) (CstInt 0)) (CstInt 42))
          @?= "(try (1 / 0) catch 42)"
    ]