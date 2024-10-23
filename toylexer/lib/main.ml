open Token
    
(* tokenize : Lexing.lexbuf -> LexingLib.Token.token list *)

let rec tokenize lexbuf =
  match Lexer.read_token lexbuf with
    EOF -> [EOF]
  | t -> t::(tokenize lexbuf)

(* lexer : string -> LexingLib.Token.token list *)

let lexer (s : string) =
  let lexbuf = Lexing.from_string s in
  tokenize lexbuf

(* string_of_tokenlist : token list -> string *)
    
let string_of_tokenlist tl = 
  List.fold_left (fun s t -> s ^ (string_of_token t ^ (if t=EOF then "" else " "))) "" tl

(* string_of_frequencies : (token * int) list -> string *)
    
let string_of_frequencies fl =
  List.fold_left (fun s (t,n) -> s ^ ((string_of_token t) ^ " -> " ^ string_of_int n ^ "\n")) "" fl

(* frequency : int -> 'a list -> ('a * int) list *)
let frequency n lst =
  
  let rec count_occurrences lst acc =
    match lst with
    | [] -> acc
    | x :: xs ->
      let count = try List.assoc x acc with Not_found -> 0 in
      let updated_acc = (x, count + 1) :: List.remove_assoc x acc in
      count_occurrences xs updated_acc
  in
  
  let token_counts = count_occurrences lst [] in
  
  let sorted_counts = List.sort (fun (_, c1) (_, c2) -> compare c2 c1) token_counts in
  
  let limited_counts = List.fold_left (fun acc x -> if List.length acc < n then x :: acc else acc) [] sorted_counts in
  List.rev limited_counts


