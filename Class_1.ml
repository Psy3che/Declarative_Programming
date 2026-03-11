(* This is an OCaml editor.
   Enter your program here and send it to the toplevel using the "Eval code"
   button or [Ctrl-e]. *)

let x=10;;
 
let sq x=x*x;; 
let sqf x=x*.x ;;
let sqi=fun x->x*x;;
let sqf2=fun x->x*.x;;
let add x y=x+y;;
let add1 x=fun y->add(x+y);;
let add2 = fun x->add1 x;;
let z=add2 10 10;;
let h=(fun f->(fun x->f(x)/.x));;
let u=h(sin)1.;;
let u2=h(sin)1.5;;
let u3=h(sin)1.8;;
let u4=h(sin)3.;;
let abs x=if x<0 then (-x) else x;;
let absf x=if x<0. then (-.x) else x;;
let rec fact n=if n<=0 then 1 else n*fact(n-1);;
let hdlist l=
  match l with
  | [h] -> h 
  | h::t -> h;;
let rec taillist l=
  match l with
  | [h] -> h
  | h::t -> taillist t;;
let rec multlist l=
  match l with
  | [h] ->h 
  | h::t->h*(multlist t);;
let rec lenlist l=
  match l with
  |[]->0
  | _ ::t->1+lenlist t;;
let rec odd l =
  match l with
  | [] -> []                
  | h::t ->                
      if h mod 2 = 1 then   
        h :: odd t          
      else
        odd t ;;   

let rec even l =
  match l with
  | [] -> []               
  | h::t ->                 
      if h mod 2 = 0 then 
        h :: even t         
      else
        even t ;;         
let rec dupl l =
  match l with
  | [] -> []               
  | h::t ->                 
      h :: h :: dupl t ;;   
