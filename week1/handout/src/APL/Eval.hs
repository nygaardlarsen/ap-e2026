module APL.Eval
  (
    Val(..), eval, envEmpty, envExtend, envLookup
  )
where

import APL.AST (Exp(..), VName) 

type Error = String

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

-- | Extend an environment with a new variable binding,
-- producing a new environment.
envExtend :: VName -> Val -> Env -> Env
envExtend n v vtable = (n, v) : vtable

-- | Look up a variable name in the provided environment.
-- Returns Nothing if the variable is not in the environment.
envLookup :: VName -> Env -> Maybe Val
envLookup n vtable = lookup n vtable

data Val
  = ValInt Integer
  | ValBool Bool
  deriving (Eq, Show)

eval :: Env -> Exp -> Either Error Val
eval env e = case e of
  (CstInt x) -> Right (ValInt x)
  Add e1 e2 -> 
    case (eval env e1, eval env e2) of 
      (Left err, _) -> Left err
      (_, Left err) -> Left err
      (Right (ValInt x), Right (ValInt y)) -> Right (ValInt (x + y))
  Sub e1 e2 ->
    case (eval env e1, eval env e2) of 
      (Left err, _) -> Left err
      (_, Left err) -> Left err
      (Right (ValInt x), Right (ValInt y)) -> Right (ValInt (x - y))
  Mul e1 e2 ->
    case (eval env e1, eval env e2) of 
      (Left err, _) -> Left err
      (_, Left err) -> Left err
      (Right (ValInt x), Right (ValInt y)) -> Right (ValInt (x * y))
  Div e1 e2 ->
    case (eval env e1, eval env e2) of 
      (Left err, _) -> Left err
      (_, Left err) -> Left err
      (_, Right (ValInt 0)) -> Left "Error: division by zero"
      (Right (ValInt x), Right (ValInt y)) -> Right (ValInt (div x y))
  Pow e1 e2 -> 
    case (eval env e1, eval env e2) of 
      (Left err, _) -> Left err
      (_, Left err) -> Left err
      (Right (ValInt x), Right (ValInt y)) -> 
        if y < 0 then Left "Error: negative exponent"
        else Right (ValInt (x ^ y))
  (CstBool b) -> Right (ValBool b)
  Eql e1 e2 ->
    case (eval env e1, eval env e2) of
      (Left err, _) -> Left err
      (_, Left err) -> Left err
      (Right (ValBool _), Right (ValInt _)) -> Left "Error: type mismatch in Eql"
      (Right (ValInt _), Right (ValBool _)) -> Left "Error: type mismatch in Eql"
      (Right (ValBool x), Right (ValBool y)) -> Right (ValBool (x == y))
      (Right (ValInt x), Right (ValInt y)) -> Right (ValBool (x == y))
  If e1 e2 e3 ->
      case (eval env e1) of
        (Left err) -> Left err
        Right (ValInt _) -> Left "Error: if statement evaluates to integer"
        Right (ValBool b) ->
          if b then
            (eval env e2)
          else
            (eval env e3)
  Var v ->
    case (envLookup v env) of
      (Just x) -> Right x
      Nothing -> Left $ "Error: Unknown variable " ++ v
  Let var e1 e2 -> 
    case (eval env e1) of
      (Left err) -> Left err
      (Right v) -> eval (envExtend var v env) e2