(* This is an OCaml editor.
   Enter your program here and send it to the toplevel using the "Eval code"
   button or [Ctrl-e]. *)

(* This is an OCaml editor.
   Enter your program here and send it to the toplevel using the "Eval code"
   button or [Ctrl-e]. *)

(*write a program that provides a range of numbers
mton m n should give [m;m+1;...;n] for specific m and n*)
(*mton 5 9 = [5;6;7;8;9]*)

let reverse l = let rec revaux l k = match l with [] -> k
                                                | h::t -> revaux t (h::k)
  in revaux l [];;

let l1 = reverse [1;2;3];;

let mton m n = let rec mnaux m n l = if m=(n+1) then reverse l 
                 else mnaux (m+1) n (m::l)
  in mnaux m n [];; 

let l2 = mton 5 9;;

(*write a function selectdiv l p that deletes the elements from l divisible by p*)
(*selectdiv [1;2;3;4;5;6] 2 = [1;3;5]*)

let rec selectdiv l p = match l with [] -> [] 
                                   | h::t -> if h mod p = 0 then selectdiv t p
                                       else h::(selectdiv t p);; 
let l3 = selectdiv [1;2;3;4;5;6] 2;;

(*write Erathostenes's sieve
sieve 10 = [2;3;5;7]*) 

let sieve n = if n<2 then failwith "no primes below 2"
  else 
    let lst = mton 2 n
    in
    let rec auxsieve k l = if k*k>n then l   
      else let h = List.hd l and u = List.hd (selectdiv l k) 
        in h::auxsieve u (selectdiv l k)
    in auxsieve 2 lst;;

let l4 = sieve 100;;

(*write gcd (greatest common divisor)*)

let rec gcd a b = if a=b then a else if a>b then gcd (a-b) b else gcd b a;;
let v1 = gcd 72 48;;
let rec gcd' a b = if b = 0 then a else let u = a mod b in gcd b u;;
let v2 = gcd' 108 96;;

(*write lcm (least common multiplier)*)
let lcm a b = a*b/(gcd a b);;
let v3 = lcm 36 48;;

let rec member n l = match l with [] -> false 
                                | h::t -> if n=h then true else member n t;;
let p1 = member 3 [1;2;3;4];;

let prime n = member n (sieve n);;

let p2 = prime 97;;

(*write another predicate prime' deciding whether a number is prime*)
(*write a function giving the prime decomposition of a given number
decompose 20 = [2;2;5]
write a function giving the prime factors of a given number
factors 20 = [2;5]
write a function listing the prime factors with exponents
primelist 20 = [(2,2);(5,1)]*)

let divide n p = n mod p = 0;; 

let prime' n = let rec aux n p = if p*p>n then true 
                 else (not (divide n p)) && aux n (p+1) 
  in aux n 2;;

let p6 = prime' 169;;

let p7 = prime' 201;;

(**Can you improve the program prime'? It should not check all values below sqrt n.*)

let decompose n = let rec aux n p list = if p>n then list 
                    else if (divide n p) then let q = n/p in aux q p (p::list)
                    else reverse (aux n (p+1) list) 
  in aux n 2 [];;

let v4 = decompose 201;;

let v5 = decompose 25;;

let v6 = decompose 12;;

(**duplicate all elements of a list
    dupl [1;2;3] = [2;4;6]*)

let rec dupl l = match l with [] -> []
                            | h::t -> 2*h :: (dupl t);;

let v7 = dupl [1;2;3];;

(**iterate all elements of a list
     iter [1;2;3] = [1;1;2;2;3;]*)

let rec iter l = match l with [] -> [] 
                            | h::t -> h::h::(iter t);;

let v8 = iter [1;2;3];;

let cleanse l = let rec first l k = match l with [] -> k
                                               | h::t -> (rest t (h::k))
  and rest l k = match l with [] -> k
                            | h::t -> let u = List.hd k in 
                                if u=h then rest t k else first l k
  in reverse (first l []);;

let l1 = cleanse [2;2;3];;

let l2 = cleanse [1;1;2;2;3;3;3];;

let prime_factors n = cleanse (decompose n);;

let l3 = prime_factors 12;;

(*[1;1;2;2;3;3;3] -> [(1,2); (2,2); (3,3)]*)

let rec prepare l =  let rec count l k = match l with [] -> k
                                                    | h::t -> (rest t ((1,h)::k))
  and rest l k = match l with [] -> k
                            | h::t -> let (n,u) = List.hd k in let w = List.tl k in  
                                if u=h then rest t ((n+1,u)::w) else count l k
  in reverse (count l []);;

let factors' n = prepare (decompose n);;
