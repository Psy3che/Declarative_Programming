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

  
  




