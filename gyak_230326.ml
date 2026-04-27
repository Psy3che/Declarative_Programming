(* This is an OCaml editor.
   Enter your program here and send it to the toplevel using the "Eval code"
   button or [Ctrl-e]. *)

(*take l n: keep the first n elements of the list
drop l n: drop the first n elements of the list*)

let rec take l n = if n=0 then [] else match l with [] -> []
                                                  | h::t -> h :: (take t (n-1));;

let l1 = take [1;2;3] 2;;

let l2 = take [1;2;3] 4;;

let rec drop l n = if n=0 then l else match l with [] -> []
                                                 | h::t -> drop t (n-1);;

let l3 = drop [1;2;3] 2;;

let l4 = drop [1;2;3] 4;;

let rec insert l h = match l with [] -> [h]
                                | u::t -> if u<h then u::(insert t h)
                                    else h::u::t;;

let rec isort l = match l with [] -> []
                             | h::t -> insert (isort t) h;;

let s1 = isort [3;7;1;4;9;5;5;3];;


let rec partition u l = let rec parti u l smaller bigger = 
                          match l with [] -> (smaller,bigger)
                                     | x::xs -> if x <= u then parti u xs (x::smaller) bigger
                                         else parti u xs smaller (x::bigger) in 
  parti u l [] [];;
                                                   

let l5 =  partition 4 [3;7;1;4;9;5;5;3];;

let rec quicksort l = match l with [] -> []
                                 | h::t -> let (u,v) = partition h t in 
                                     let l' = quicksort u in 
                                     let l'' =  quicksort v in 
                                     l' @ [h] @ l'';;

let rec merge k l = match k with [] -> l
                               | h::t ->
                                   match l with [] -> k
                                              | i::s -> if h <= i 
                                                  then h::(merge t l)
                                                  else i::(merge k s);;

let l6 = merge [1;3;5] [2;3;6;8];;

let rec msort l = match l with [] -> []
                             | [h] -> [h] 
                             | h::t -> let rec llength k = 
                                         match k with [] -> 0
                                                    | _::s -> 1+llength s
                                 in
                                 let len = llength l in
                                 let half = len / 2 in
                                 let l' = take l half in
                                 let l'' = drop l half in
                                 merge (msort l') (msort l'');;

let l7 = msort [3;1;9;4;7;5] ;;

let rec bubble l = match l with h::t -> let rec bub max k p = 
                                          match k with [] -> (max, p)
                                                     | q::s -> 
                                                         if q <= max 
                                                         then bub max s (q::p)
                                                         else bub q s (max::p)
  in bub h t [];;

let (u,v) = bubble [5;8;1;3;7];;
let (u,v) = bubble [5;8;1;8;4;7];;

let rec bubblesort l = match l with [] -> []
                                  | h::t -> 
                                      let (u,v) = bubble l in
                                      bubblesort v @ [u];;
let l8 = bubblesort [5;8;1;9;4;7] ;;