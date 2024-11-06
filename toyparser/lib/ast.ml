type ast =
    Const of int
  | Neg of ast
  | Mul of ast * ast
  | Div of ast * ast
  | Add of ast * ast
  | Sub of ast * ast
  

