(*Task1 :实现一个函数 mton m n，它返回从 m 到 n（包含两端）的整数列表 [m; m+1; ...; n]。*)
let reverse l =
  let rec revaux l k = match l with
    | [] -> k
    | h::t -> revaux t (h::k)
  in revaux l [];;

let l1 = reverse [1;2;3];;  (* 结果为 [3; 2; 1] *)

let mton m n =
  let rec mnaux m n l =
    if m = (n+1) then reverse l
    else mnaux (m+1) n (m::l)
  in mnaux m n [];;

let l2 = mton 5 9;;  (* 结果为 [5; 6; 7; 8; 9] *)
                     
                     
(* Task 2: 生成从 m 到 n 的整数列表 *)
let mton m n =
  (* 辅助函数：尾递归构建逆序列表，最后反转 *)
  let rec mnaux m n l =
    if m = (n+1) then reverse l  (* 终止条件：反转得到正确顺序 *)
    else mnaux (m+1) n (m::l)    (* 把当前 m 加入列表，m 自增 *)
  in mnaux m n [];;              (* 初始列表为空 *)

(* 测试示例 *)
let l2 = mton 5 9;;              (* 输出：val l2 : int list = [5; 6; 7; 8; 9] *)
                          
                                 
                                                                                          
(* Task 3: 过滤掉列表中能被 p 整除的元素 *)
let rec selectdiv l p = match l with
  | [] -> []                                    (* 空列表直接返回 *)
  | h::t -> 
      if h mod p = 0 then selectdiv t p         (* 能被 p 整除则跳过 *)
      else h::(selectdiv t p)                   (* 不能整除则保留元素 *)

(* 测试示例 *)
let l3 = selectdiv [1;2;3;4;5;6] 2;;            (* 输出：val l3 : int list = [1; 3; 5] *)
                                                
                                                
                                                
(* Task 4: 埃拉托斯特尼筛法（素数筛选） *)
(*write Erathostenes's sieve
sieve 10 = [2;3;5;7]*)

let sieve n = if n<2 then failwith "no primes below 2"
  else
    let lst = mton 2 n
    in
    let rec auxsieve k l = if k*k>n then l
      else auxsieve (k+1) (selectdiv l k)
    in auxsieve 2 lst;;

let 14 = sieve 30;;


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
decompose 20 = [2;2;5]*)
(*write a function giving the prime factors of a given number
factors 20 = [2;5]*)
(*write a function listing the prime factors with exponents
primelist 20 = [(2,2);(5,1)]*)