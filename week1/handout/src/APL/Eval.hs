module APL.Eval
  (
    Val(..), eval
  )
where

import APL.AST (Exp(..))

type Error = String

data Val
  = ValInt Integer
  | ValBool Bool
  deriving (Eq, Show)

eval :: Exp -> Either Error Val
eval e = case e of
  (CstInt x) -> Right (ValInt x)
  Add e1 e2 -> 
    case (eval e1, eval e2) of 
      (Left err, _) -> Left err
      (_, Left err) -> Left err
      (Right (ValInt x), Right (ValInt y)) -> Right (ValInt (x + y))
  Sub e1 e2 ->
    case (eval e1, eval e2) of 
      (Left err, _) -> Left err
      (_, Left err) -> Left err
      (Right (ValInt x), Right (ValInt y)) -> Right (ValInt (x - y))
  Mul e1 e2 ->
    case (eval e1, eval e2) of 
      (Left err, _) -> Left err
      (_, Left err) -> Left err
      (Right (ValInt x), Right (ValInt y)) -> Right (ValInt (x * y))
  Div e1 e2 ->
    case (eval e1, eval e2) of 
      (Left err, _) -> Left err
      (_, Left err) -> Left err
      (_, Right (ValInt 0)) -> Left "Error: division by zero"
      (Right (ValInt x), Right (ValInt y)) -> Right (ValInt (div x y))
  Pow e1 e2 -> 
    case (eval e1, eval e2) of 
      (Left err, _) -> Left err
      (_, Left err) -> Left err
      (Right (ValInt x), Right (ValInt y)) -> 
        if y < 0 then Left "Error: negative exponent"
        else Right (ValInt (x ^ y))
  (CstBool b) -> Right (ValBool b)
  Eql e1 e2 ->
    case (eval e1, eval e2) of
      (Left err, _) -> Left err
      (_, Left err) -> Left err
      (Right (ValBool _), Right (ValInt _)) -> Left "Error: type mismatch in Eql"
      (Right (ValInt _), Right (ValBool _)) -> Left "Error: type mismatch in Eql"
      (Right (ValBool x), Right (ValBool y)) -> Right (ValBool (x == y))
      (Right (ValInt x), Right (ValInt y)) -> Right (ValBool (x == y))
  If e1 e2 e3 ->
      case (eval e1) of
        (Left err) -> Left err
        Right (ValInt _) -> Left "Error: if statement evaluates to integer"
        Right (ValBool b) ->
          if b then
            (eval e2)
          else
            (eval e3)