type token =
  | LPAREN
  | RPAREN
  | ASSIGN
  | PLUS
  | SEQ
  | ID of string  (*ID + il tipo di ciò che contiene*)
  | CONST of string
  | EOF
  | ATOK of string
  | BTOK of string
  | CTOK of string
  | DTOK of string
  | ETOK of string


  (* Aggiungi token task 2*)

let string_of_token = function
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | ASSIGN -> "ASSIGN"
  | PLUS -> "PLUS"
  | SEQ -> "SEQ"
  | ID(s) -> "ID(" ^ s ^ ")"
  | CONST(s) -> "CONST(" ^ s ^ ")"
  | EOF -> "EOF"
  | ATOK(s) -> "ATOK(" ^ s ^ ")"
  | BTOK(s) -> "BTOK(" ^ s ^ ")"
  | CTOK(s) -> "CTOK(" ^ s ^ ")"
  | DTOK(s) -> "DTOK(" ^ s ^ ")"
  | ETOK(s) -> "ETOK(" ^ s ^ ")"

