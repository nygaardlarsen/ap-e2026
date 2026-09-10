module APL.AST
  (
    Exp(..)
  )
where

data Exp
  = CstInt Integer
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Pow Exp Exp
  | CstBool Bool
  | Eql Exp Exp
  | If Exp Exp Exp
  deriving (Eq, Show)