



let rec lang1 l = 
  match l with
  | [] -> false
  | x::[] when (x='0' || x='1') -> true
  | h::t when (h='0' || h='1') -> lang1 t
  | _ -> false;; 

let rec lang2 l =
  match l with
  | [] -> true
  | x::[] when x='1' -> true
  | h::x::t when (h='0' && x<>'0') -> lang2 t
  | h::t when (h='1') -> lang2 t
  | _ -> false
;;


let check l =
  List.hd l = '0' && List.hd (List.rev l) = '0';;

let lang3 l =
  if check l then
    List.fold_left (fun acc x -> if (x='0' || x = '1') then acc else false) true l
  else false
;;

let rec check_only_01 l =
  match l with
  |[] -> false
  | x::[] when (x='1' || x='0') -> true
  |h::t when (h='1' || h='0') -> check_only_01 t
  | _ -> false
;;


let lang4 l = 
  if check_only_01 l then
    if List.fold_left (fun acc x -> if x='1' then acc+1 else acc ) 0 l <> 2
      then false
      else true
  else false
;;


let rec lang5 l =
  match l with
  | [] -> true                          
  | '0'::'0'::t -> lang5 t     
  | '1'::'1'::t -> lang5 t     
  | _ -> false                          
;;
    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
  
