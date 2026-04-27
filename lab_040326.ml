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
