type 'a tree =
    Br of 'a * 'a tree * 'a tree
  | Lf


(*type inttr =
   Nd of int * inttr * inttr
  | L  *)

let u =  Br (3, Lf, Lf);;


let rec size tr =
  match tr with
    Br (_, l, r) -> 1 + size l + size r
  | Lf -> 0;;


(*1. write sumtree
2. height of a tree*)

let v = Br (7, Br (3, Lf, Lf), Br (5, Lf, Lf));;

let rec sumtree tr = 
  match tr with
    Br(n, l, r) -> n + (sumtree l) + (sumtree r)
  | Lf -> 0;;

let max x y = if x>y then x else y;;

let rec hth tr  = 
  match tr with
    Br(_, l, r) -> 1 + max (hth l) (hth r)
  | Lf -> 0;;


(*
find the leftmost branch of a tree,
find the maximal element of a tree,
find the reflection of a tree on its vertical axis,
compare two trees,
find the branch of a tree the sum of the elements of which is maximal,
find the levels of a tree the sum of the elements of which is a prime number,
find a branch of a tree the sum of the elements of which is a prime number,
find the branches of a tree the sum of the elements of which is a prime number,
insert an element into a tree,
delete an element,
write tree_from_list and list_from_tree
write the inorder, preorder, postorder traversals of a tree*)

let rec leftmost tr = 
  match tr with
    Br(n, l, r) -> n:: leftmost l
  | Lf -> [];;

let rec maxtree tr = 
  match tr with
    Br(n, l, r) -> let m = max (maxtree l) (maxtree r) in 
      max n m
  | Lf -> 0;;

(*find the branch of a tree the sum of the elements of which is maximal*)

let rec sumlist l: int  = 
  match l with [] -> 0
             | h::t -> h + sumlist t;;

let rec maxsum tr = 
  match tr with
    Br(n, l, r) -> let (u1,u2) = (maxsum l, maxsum r) in
      let (m1, m2) = (sumlist u1, sumlist u2) in 
      if m1 > m2 then n :: u1 else n :: u2
  | Lf -> [];; 

(*find a branch of a tree the sum of the elements of which is a prime number,
find the branches of a tree the sum of the elements of which is a prime number*)

(*insert an element into a tree*)

let rec  treeins t k = 
  match t with
    Br(n, l, r) -> Br(n, treeins l k, r)
  | Lf -> Br(k, Lf, Lf);;

(*build a tree from a list using treeins*)
(*transplant a subtree into a tree in place of a leaf element*)
(*build a balanced tree with transplantation*)

let rec buildtr l = 
  let rec aux l t =
    match l with [] -> t
               | h::s -> aux s (treeins t h)
  in
  aux l Lf;;


  
  




    
    
  