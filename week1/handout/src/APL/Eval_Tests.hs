module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Val (..), eval, envEmpty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10]

test1 :: TestTree
test1 = testCase "Integer test" $ eval envEmpty (CstInt 10) @?= Right (ValInt 10)

test2 :: TestTree
test2 = testCase "Addition test" $ eval envEmpty (Add (CstInt 10) (CstInt 18)) @?= Right (ValInt 28)

test3 :: TestTree
test3 = testCase "Subtraction test" $ eval envEmpty (Sub (CstInt 10) (CstInt 4)) @?= Right (ValInt 6)

test4 :: TestTree
test4 = testCase "Multiplication test" $ eval envEmpty (Mul (CstInt 10) (CstInt 18)) @?= Right (ValInt 180)

test5 :: TestTree
test5 = testCase "Division test" $ eval envEmpty (Div (CstInt 18) (CstInt 9)) @?= Right (ValInt 2)

test6 :: TestTree
test6 = testCase "Power test" $ eval envEmpty (Pow (CstInt 10) (CstInt 2)) @?= Right (ValInt 100)

test7 :: TestTree
test7 = testCase "Division test fail" $ eval envEmpty (Div (CstInt 10) (CstInt 0)) @?= Left "Error: division by zero"

test8 :: TestTree
test8 = testCase "Division power fail" $ eval envEmpty (Pow (CstInt 10) (CstInt (-8))) @?= Left "Error: negative exponent"

test9 :: TestTree
test9 = testCase "Division power fail" $ eval envEmpty (Let "x" (CstInt 5) (Var "x")) @?= Right (ValInt 5)

test10 :: TestTree
test10 = testCase "Division power fail" $ eval envEmpty (Var "x") @?= Left "Error: Unknown variable x"


