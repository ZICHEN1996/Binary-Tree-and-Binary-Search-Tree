module Expression (
    Expression (Number, Node),
    Operand (Plus, Minus, Times, Divided_By, Power),
    eval -- :: Expression -> Expression
) where
data Expression a
  = Number a
  | Node (Expression a)
         Operand
         (Expression a)
  deriving (Show, Eq)
data Operand
  = Plus
  | Minus
  | Times
  | Divided_By
  | Power
  deriving (Show, Eq)
eval
  :: Floating a
  => Expression a -> Expression a
eval (Number n) = Number n
eval (Node (Number e1) Plus (Number e2)) = Number (e1 + e2)
eval (Node (Number e1) Minus (Number e2)) = Number (e1 - e2)
eval (Node (Number e1) Times (Number e2)) = Number (e1 * e2)
eval (Node (Number e1) Divided_By (Number e2)) = Number (e1 / e2)
eval (Node (Number e1) Power (Number e2)) = Number (e1 ** e2)
eval (Node e1 Plus e2) = eval (Node (eval e1) Plus (eval e2))
eval (Node e1 Minus e2) = eval (Node (eval e1) Minus (eval e2))
eval (Node e1 Times e2) = eval (Node (eval e1) Times (eval e2))
eval (Node e1 Divided_By e2) = eval (Node (eval e1) Divided_By (eval e2))
eval (Node e1 Power e2) = eval (Node (eval e1) Power (eval e2))