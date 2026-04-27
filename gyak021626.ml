(* This is an OCaml editor.
   Enter your program here and send it to the toplevel using the "Eval code"
   button or [Ctrl-e]. *)

let rec lth1 list = match list with
  | [] -> 0
  | hd :: tl -> 1 + (lth1 tl);;

let rec lthrec list = 
  let rec aux l lg = match l with
    | [] -> lg
    | p::s -> aux s (lg +1)
  in 
  aux list 0;;

let rec sumlist l = if l = [] then 0 
  else let p = List.hd l and s = List.tl l in
    p + sumlist s;;

let rec sumrec list = 
  let rec aux l sum = match l with
    | [] -> sum
    | p::t -> aux t (sum + p)
  in
  aux list 0;;

(*lista elemeinek szorzata kétféle módon: simán rekurzív és végrekurzív 
(tail recursive) formában*)

(*feladat: fstlst lista első eleme
lastlst lista utolsó eleme*)

let fstlst l = if l = [] then failwith "empty list"
  else let p = List.hd l in
    p;;

let rec lastlst l = if l = [] then failwith "empty list"
  else let p = List.hd l and q = List.tl l in
    if q = [] then p else lastlst q;;

let rec last2 l = match l with
  | [] -> failwith "empty list"
  | [p] -> p
  | p::q -> last2 q;;

let rec rev l = match l with 
  | [] -> []
  | p::t -> (rev t) @ [p];;

let rec revit list = let rec aux l tmp = 
                       match l with
                       | [] -> tmp
                       | p::t -> aux t (p::tmp)
  in
  aux list [];;

let rec myappend l1 l2 = 
  let rec aux list1 list2 = match list1 with
    | [] -> list2
    | p::t -> p :: (aux t list2)
  in
  aux l1 l2;;

(*listázzuk ki a 
1. páros indexű listaelemeket
2. listázzuk ki a páratlan listaelemeket
3. hagyjuk el a 7 minden előfordulását egy listából
4. hagyjuk el a 7 második előfordulását egy listából
5. hagyjuk el a 7 minden második előfordulását egy listából
6. töröljük a 7 minden ismétlődését egy listából, 
azaz [1;7;7;6;7] -> [1;7;6;7]*)


