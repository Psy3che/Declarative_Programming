(* This is an OCaml editor.
   Enter your program here and send it to the toplevel using the "Eval code"
   button or [Ctrl-e]. *)

(*recursive function for exponentation
pow x 0 = 1
pow x (y+1) = x * (pow x y)*)

let rec pow x y = if y=0 then 1 else x* (pow x (y-1));;

(*tail recursive manner*)

let rec pow_tr x y = if y = 0 then 1 else let rec powaux x y acc = 
                                            if y = 0 then acc else powaux x (y-1) x*acc
    in
    powaux x y 1;;

let rec powTr x y = let rec powAux x y z acc = if y=z then acc else
                        powAux x y (z+1) x*acc
  in powAux x y 0 1;;

(*del5 deletes the first occurrence of 5 from a list*)

let rec del5 lst = match lst with [] -> [] | h::t -> if h=5 then t else h::(del5 t);;

let l = del5 [1;5;5;2;7;5;8];;

(*delx deletes the first occurrence of x from a list*)

let rec delx lst x = match lst with [] -> [] | h::t -> if h=x then t else h::(delx t x);;

let del7 lst = delx lst 7;;

let l' = del7 [1;5;5;2;7;5;8;7];;

(*delete all occurrences of 5 from a list*)

let rec delall5 lst = match lst with [] -> [] 
                                   | h::t -> if h=5 then delall5 t else h::(delall5 t);;

let l'' = delall5 [1;5;5;2;7;5;8;7];;


let rec reverse l = match l with [] -> []
                               | h::t -> reverse t @ [h];;

let rec rev_tr l = let rec revaux l acc = match l with [] -> acc
                                                     | h::t -> revaux t (h::acc)
  in
  revaux l [];;

let l4 = rev_tr [1;5;5;2;7;5;8;7];;

let rec delall5_tr lst = let rec del5aux lst acc = 
                           match lst with [] -> acc 
                                        | h::t -> if h=5 then del5aux t acc 
                                            else del5aux t (h::acc) 
  in 
  rev_tr (del5aux lst []);;

   
let l''' = delall5_tr [1;5;5;2;7;5;8;7];; 

let rec myappend lst1 lst2 = match lst1 with
  | [] -> lst2
  | h::t -> h :: (myappend t lst2);;

let l5 = myappend [1;2;3] [4;5;6];;

(*find the last element of a list*) 

let tail lst = match lst with
  | _::t -> t;;

let rec lastlst l = match l with
  | [] -> failwith "empty list"
  | [x] -> x
  | _ :: t -> lastlst t;;

let v1 = lastlst [1;2;3;4;5];;

(*delete every second occurrence of 5 from a list*)

let rec del5even lst = let rec auxeven lst = match lst with
    | [] -> []
    | h::t -> if h=5 then auxodd t else h :: (auxodd t) and
  auxodd lst = match lst with
    | [] -> []
    | h::t -> if h=5 then h :: (auxeven t) else h :: (auxodd t)
  in
  auxodd lst;;

let l7 = del5even [1;5;3;5;7;5;9;5] ;;
