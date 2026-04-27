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


let rec partition u l = let rec parti u l smaller bigger = match l with [] -> (smaller,bigger)
                                                  | x::xs -> if x <= u then parti u xs (x::smaller) bigger
                                                      else parti u xs smaller (x::bigger) in 
                                                    parti u l [] [];;
                                                   

let l5 =  partition 4 [3;7;1;4;9;5;5;3];;

let rec quicksort l = match l with [] -> []
| h::t -> let (u,v) = partition h t in 
let l' = quicksort u in 
let l'' =  quicksort v in 
l' @ [h] @ l'';;


