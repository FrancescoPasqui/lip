open Ast

(* parse : string -> ast *)

let parse (s : string) : ast =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read_token lexbuf in
  ast

type result = int



let string_of_result n = string_of_int n
    
(* eval : ast -> result *)
    
let rec eval = function
    Const(n) -> n
  | Neg(e) -> eval e * (-1)
  | Mul(e1,e2) -> eval e1 * eval e2
  | Div(e1,e2) -> if eval e2 = 0 then failwith "None" else eval e1 / eval e2
  | Add(e1,e2) -> eval e1 + eval e2
  | Sub(e1,e2) -> eval e1 - eval e2
  

                    
