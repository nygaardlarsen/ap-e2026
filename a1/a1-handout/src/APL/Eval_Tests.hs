module APL.Eval_Tests (tests) where

import APL.AST (Exp (..), printExp)
import APL.Eval (Val (..), envEmpty, eval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- -- Consider this example when you have added the necessary constructors.
-- -- The Y combinator in a form suitable for strict evaluation.
yComb :: Exp
yComb =
  Lambda "f" $
    Apply
      (Lambda "g" (Apply (Var "g") (Var "g")))
      ( Lambda
          "g"
          ( Apply
              (Var "f")
              (Lambda "a" (Apply (Apply (Var "g") (Var "g")) (Var "a")))
          )
      )

fact :: Exp
fact =
  Apply yComb $
    Lambda "rec" $
      Lambda "n" $
        If
          (Eql (Var "n") (CstInt 0))
          (CstInt 1)
          (Mul (Var "n") (Apply (Var "rec") (Sub (Var "n") (CstInt 1))))

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [ testCase "Add" $
        eval envEmpty (Add (CstInt 2) (CstInt 5))
          @?= Right (ValInt 7),
      --
      testCase "Add (wrong type)" $
        eval envEmpty (Add (CstInt 2) (CstBool True))
          @?= Left "Non-integer operand",
      --
      testCase "Sub" $
        eval envEmpty (Sub (CstInt 2) (CstInt 5))
          @?= Right (ValInt (-3)),
      --
      testCase "Div" $
        eval envEmpty (Div (CstInt 7) (CstInt 3))
          @?= Right (ValInt 2),
      --
      testCase "Div0" $
        eval envEmpty (Div (CstInt 7) (CstInt 0))
          @?= Left "Division by zero",
      --
      testCase "Pow" $
        eval envEmpty (Pow (CstInt 2) (CstInt 3))
          @?= Right (ValInt 8),
      --
      testCase "Pow0" $
        eval envEmpty (Pow (CstInt 2) (CstInt 0))
          @?= Right (ValInt 1),
      --
      testCase "Pow negative" $
        eval envEmpty (Pow (CstInt 2) (CstInt (-1)))
          @?= Left "Negative exponent",
      --
      testCase "Eql (false)" $
        eval envEmpty (Eql (CstInt 2) (CstInt 3))
          @?= Right (ValBool False),
      --
      testCase "Eql (true)" $
        eval envEmpty (Eql (CstInt 2) (CstInt 2))
          @?= Right (ValBool True),
      --
      testCase "If" $
        eval envEmpty (If (CstBool True) (CstInt 2) (Div (CstInt 7) (CstInt 0)))
          @?= Right (ValInt 2),
      --
      testCase "Let" $
        eval envEmpty (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= Right (ValInt 5),
      --
      testCase "Let (shadowing)" $
        eval
          envEmpty
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= Right (ValBool True),
          --
      testCase "ForLoop standard" $
        eval envEmpty (ForLoop ("p", CstInt 0) ("i", CstInt 10) (Add (Var "p") (Var "i")))
          @?= Right (ValInt 45),
          --
      testCase "ForLoop wrong bound type" $
        eval envEmpty (ForLoop ("p", CstInt 0) ("i", CstBool True) (Var "p"))
          @?= Left "Non-integer loop bound",

      testCase "ForLoop zero iterations" $
        eval envEmpty (ForLoop ("p", CstInt 7) ("i", CstInt 0) (Add (Var "p") (CstInt 1)))
          @?= Right (ValInt 7),

      testCase "ForLoop negative bound" $
        eval envEmpty (ForLoop ("p", CstInt 7) ("i", CstInt (-3)) (Add (Var "p") (CstInt 1)))
          @?= Right (ValInt 7),

      testCase "ForLoop body error" $
        eval envEmpty (ForLoop ("p", CstInt 0) ("i", CstInt 2) (Div (CstInt 1) (CstInt 0)))
          @?= Left "Division by zero",

      testCase "ForLoop initial error" $
        eval envEmpty (ForLoop ("p", Div (CstInt 1) (CstInt 0)) ("i", CstInt 2) (Var "p"))
          @?= Left "Division by zero",

      testCase "Lambda application" $
        eval envEmpty (Apply (Lambda "x" (Add (Var "x") (CstInt 1))) (CstInt 5))
          @?= Right (ValInt 6),

      testCase "Lambda captures environment" $
        eval envEmpty (Let "x" (CstInt 2) (Apply (Lambda "y" (Add (Var "x") (Var "y"))) (CstInt 3)))
          @?= Right (ValInt 5),

      testCase "Lambda uses lexical scoping" $
        eval envEmpty
          (Let "x" (CstInt 10)
            (Let "f" (Lambda "y" (Add (Var "x") (Var "y")))
              (Let "x" (CstInt 100)
                (Apply (Var "f") (CstInt 1)))))
          @?= Right (ValInt 11),
            
      testCase "Apply function argument" $
        eval envEmpty
          (Apply
            (Lambda "f" (Apply (Var "f") (CstInt 3)))
            (Lambda "x" (Add (Var "x") (CstInt 1))))
          @?= Right (ValInt 4),

      testCase "Apply non-function int" $
        eval envEmpty (Apply (CstInt 5) (CstInt 2))
          @?= Left "Error: apply evaluated to ValInt",

      testCase "Apply non-function bool" $
        eval envEmpty (Apply (CstBool True) (CstInt 2))
          @?= Left "Error: apply evaluated to ValBool",

      testCase "Apply" $
        eval envEmpty (Apply (Lambda "x" (Add (Var "x") (CstInt 1))) (CstInt 5))
          @?= Right (ValInt 6),

      testCase "Apply boolean argument" $
        eval envEmpty (Apply (Lambda "x" (Var "x")) (CstBool True))
          @?= Right (ValBool True),

      testCase "Apply evaluates function first" $
        eval envEmpty
          (Apply
            (Div (CstInt 1) (CstInt 0))
            (Pow (CstInt 2) (CstInt (-1))))
          @?= Left "Division by zero",
          
      testCase "Apply accepts function argument" $
        eval envEmpty
          (Apply
            (Lambda "f" (Apply (Var "f") (CstInt 3)))
            (Lambda "x" (Add (Var "x") (CstInt 1))))
          @?= Right (ValInt 4),

      testCase "Closure" $
        eval envEmpty (Let "x" (CstInt 2) (Apply (Lambda "y" (Add (Var "x") (Var "y"))) (CstInt 3)))
          @?= Right (ValInt 5),

      testCase "Closure ignores later shadowing" $
        eval envEmpty
          (Let "x" (CstInt 10)
            (Let "f" (Lambda "y" (Add (Var "x") (Var "y")))
              (Let "x" (CstInt 100)
                (Apply (Var "f") (CstInt 1)))))
          @?= Right (ValInt 11),

      testCase "TryCatch success" $
        eval envEmpty (TryCatch (CstInt 5) (CstInt 10))
          @?= Right (ValInt 5),

      testCase "TryCatch error" $
        eval envEmpty (TryCatch (Div (CstInt 1) (CstInt 0)) (CstInt 10))
          @?= Right (ValInt 10),

      testCase "TryCatch skips catch on success" $
        eval envEmpty
          (TryCatch
            (CstInt 5)
            (Div (CstInt 1) (CstInt 0)))
          @?= Right (ValInt 5),

      testCase "TryCatch evaluates catch on error" $
        eval envEmpty
          (TryCatch
            (Div (CstInt 1) (CstInt 0))
            (CstInt 42))
          @?= Right (ValInt 42),

      testCase "Nested TryCatch catches inner failure" $
        eval envEmpty
          (TryCatch
            (TryCatch
              (Div (CstInt 1) (CstInt 0))
              (Pow (CstInt 2) (CstInt (-1))))
            (CstInt 99))
          @?= Right (ValInt 99),
      testCase "Factorial" $
        eval envEmpty (Apply fact (CstInt 5))
          @?= Right (ValInt 120)
                -- TODO - add more
    ]
