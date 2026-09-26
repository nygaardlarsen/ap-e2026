module APL.Eval
  ( Val (..),
    eval,
    runEval,
    Error,
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  deriving (Eq, Show)

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup v env = lookup v env

type Error = String

newtype EvalM a = EvalM (Either Error a)

-- fmap :: (a -> b) -> EvalM a -> EvalM b
instance Functor EvalM where
  fmap _ (EvalM (Left err)) = (EvalM (Left err))
  fmap f (EvalM (Right x)) = 
    let y = f x in
      EvalM (Right y)


-- pure :: Applicative EvalM => a - EvalM a
-- (<*>) :: Applicative EvalM => EvalM (a -> b) -> EvalM a -> EvalM b

instance Applicative EvalM where
  pure a = EvalM (Right a)

  (<*>) (EvalM f) (EvalM a) = case f of
    Left err -> EvalM (Left err)
    Right func -> case a of
      Left err -> EvalM (Left err)
      Right x -> EvalM (Right (func x))


-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
instance Monad EvalM where
    (>>=) (EvalM a) f = case a of
      Left err -> EvalM (Left err)
      Right x -> f x


runEval :: EvalM a -> Either Error a
runEval (EvalM a) = a

eval :: Env -> Exp -> EvalM Val
eval env (CstInt a) = pure (ValInt a)
eval env (CstBool b) = pure (ValBool b)
eval env (Var v) = do
  case envLookup v env of
    Just x -> pure x
    Nothing -> failure ("Variable not found: " ++ v)
eval env (Add e1 e2) = do
  x <- eval env e1
  y <- eval env e2
  case (x,y) of
    (ValInt x', ValInt y') -> pure (ValInt (x' + y'))
    _ -> failure "Error: attempted to add non integers"
eval env (Sub e1 e2) = do
  x <- eval env e1
  y <- eval env e2
  case (x,y) of
    (ValInt x', ValInt y') -> pure (ValInt (x' - y'))
    _ -> failure "Error: attempted to subtract non integers"
eval env (Mul e1 e2) = do
  x <- eval env e1
  y <- eval env e2
  case (x,y) of
    (ValInt x', ValInt y') -> pure (ValInt (x' * y'))
    _ -> failure "Error: attempted to multiply non integers"
eval env (Div e1 e2) = do
  x <- eval env e1
  y <- eval env e2
  case (x,y) of
    (ValInt x', ValInt 0) -> failure "Error division by zero"
    (ValInt x', ValInt y') -> pure (ValInt (x' `div` y'))
    _ -> failure "Error: attempted to divide non integers"
eval env (TryCatch e1 e2) =
  eval env e1 `catch` eval env e2

failure :: String -> EvalM a
failure s = EvalM (Left s)


catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) = EvalM $
  case m1 of
    Right x -> Right x
    Left _ -> m2 