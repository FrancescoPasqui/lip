{
  open Token
}

(*Espressioni regolari*)
let white = [' ' '\t']+
let btok = ['a' 'e' 'i' 'o' 'u']+
let ctok = [^'a''e''i''o''u''A''E''I''O''U' '0'-'9']*['a''e''i''o''u''A''E''I''O''U'][^'a''e''i''o''u''A''E''I''O''U' '0'-'9']*
let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let atok = ['A'-'Z'] chr*
let id = letter chr* 
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let dtok =  ['-'] ['.']? num ['.']? num* | num ['.'] num*
let etok = ['0']['x' 'X'] ['a'-'f' 'A'-'F' '0'-'9']+


rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "=" { ASSIGN }
  | "+" { PLUS }
  | ";" { SEQ }
  | btok {BTOK (Lexing.lexeme lexbuf)}  
  | ctok {CTOK (Lexing.lexeme lexbuf)} 
  | atok {ATOK (Lexing.lexeme lexbuf)}
  | id { ID (Lexing.lexeme lexbuf) }   (*(Lexing.lexeme lexbuf): mette tra parentesi quello che viene dopo*)
  | num { CONST (Lexing.lexeme lexbuf) }  
  | dtok {DTOK (Lexing.lexeme lexbuf)}  
  | etok {ETOK (Lexing.lexeme lexbuf)}
  | eof { EOF }

