(* This is an OCaml editor.
   Enter your program here and send it to the toplevel using the "Eval code"
   button or [Ctrl-e]. *)

(* This is an OCaml editor.
   Enter your program here and send it to the toplevel using the "Eval code"
   button or [Ctrl-e]. *)

(**hatványozás: power x y = x^y
rekurzív: power x 0 = 1
   power x (y+1) = x* (power x y)
*)

let rec power x y = if y=0 then 1 else x* (power x (y-1));;

let rec pow_tr x y = if y=0 then 1 else let rec pow_aux x y acc = 
                                          if y=0 then acc else pow_aux x (y-1) x*acc  
    in
    pow_aux x y 1;;

let rec powTr x y = let rec powAux x y z acc = if z=y then acc else 
                        powAux x y (z+1) x*acc
  in powAux x y 0 1;;

let rec even n = n=0 || odd (n-1) and 
  odd n = n <> 0 && even (n-1);;

let multi ~name1:name1 ~name2:name2 = name1 * name2;;

let multiop ?name1:(x1=2) x2 = x1*x2;;

let add x y = x+y;;

let addx y = fun x -> x + y;;

let addxy = (fun x -> (fun y -> x+y));;

let uncurry f (x,y) = f x y;;

(**uncurry add (2,3);;*)

let curry f x y = f (x,y);;

let adduc (x,y) = x+y;;

(**curry adduc 2 3;;*)

let rec revnaiv l = match l with [] -> [] | h::t -> (revnaiv t) @ [h];;

let rec rev_tr l = let rec revaux l acc = 
                     match l with [] -> acc | h::t -> revaux t (h::acc)
  in revaux l [];;

(**rekurzív és farokrekurzív megoldások megkeresése:     
1. lista páratlan indexű elemeinek megtartása,
2. lista 2-vel és 3-mal osztható elemeinek megtartása,
3. lista 2-vel vagy 3-mal osztható elemeinek megtartása,
4. adjuk meg egy számhoz relatív prím, tőle kisebb elemek listáját
5. adjuk meg egy számnál kisebb ikerprímek listáját (p, q ikerprím, 
   ha p és q prím, és |p-q|=2),
6.  adjuk meg egy szám valódi osztói listáját,
7. adjuk meg egy szám prímtényezős ferlbontását
8. adjuk meg két szám legkisebb közös többszörösét.
*)

let rec even n = n=0 || odd (n-1) and odd n = even (n-1) && n<>0;;

let rec oddlist l = match l with [] -> []
                               | h::t -> h :: (evenlist t) 
and evenlist l = match l with [] -> []
                            | h::t -> oddlist t;;

let rec even l = if l=[] then [] else let h = List.hd l and 
    t = List.tl l
    in if h mod 2 =0 then h :: (even t) else even t;;

let rec twoORthree l = if l=[] then [] else let h = List.hd l and 
    t = List.tl l
    in if h mod 2 = 0 && h mod 2 = 0 then h :: (twoORthree t) else twoORthree t;;

let rec gcd a b = if a=b then a else if a>b then gcd (a-b) b else gcd b a;;

let rec gcd' a b = if b=0 then a else let rmd = a mod b in gcd' b rmd;; 

let rec mton m n = let rec fromto m n lst = if m=n+1 then lst else 
                       fromto (m+1) n (m::lst)
  in rev_tr (fromto m n []);;

let x1 = mton 1 10;;
let x2 = mton 1 40;;

let rec deldiv l p = match l with [] -> [] 
                                | h::t -> if h mod p = 0 then deldiv t p 
                                    else  h :: (deldiv t p);;

let x3 = deldiv [2;4;6;8;12;17] 3 ;;
let x4 = deldiv x2 3 ;;

let rec erath n = if n=1 then failwith "no primes below 2" else 
    let  init = mton 2 n
    in
    let rec sieve l = match l with [h] -> [h]
                                 | h::t -> h :: sieve (deldiv t h)
    in sieve init;;






 
  




