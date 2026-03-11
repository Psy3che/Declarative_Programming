(* 辅助函数：判断 n 是否能被 p 整除 *)
let divide n p = n mod p = 0;;

(* 判断素数的函数 prime' *)
let prime' n =
  let rec aux n p =
    if p*p > n then true
    else (not (divide n p)) && aux n (p+1)
  in aux n 2;;

let p2 = prime' 97;;
let p3 = divide 121 11;;
let p4 = prime' 101;;
let p5 = prime' 105;;
let p6 = prime' 169;;
let p7 = prime' 201;;

(* 质因数分解函数 decompose *)
let decompose n =
  let rec aux n p list =
    if p*p > n then list
    else if (divide n p) then
      let q = n / p in
      aux q p (p::list)
    else aux n (p+1) list
  in List.rev (aux n 2 []);;

let v4 = decompose 201;;
let v5 = decompose 25;;
let v6 = decompose 12;;

(* 列表元素翻倍函数 dupl *)
(* dupl [1;2;3] = [2;4;6] *)
let rec dupl l = match l with
  | [] -> []
  | h::t -> 2*h :: (dupl t);;

let v7 = dupl [1;2;3];;

(* 列表元素迭代函数 iter *)
(* iter [1;2;3] = [1;1;2;2;3;3] *)
let rec iter l = match l with
  | [] -> []
  | h::t -> h::h::(iter t);;

let v8 = iter [1;2;3];;

(* 清理相邻重复元素函数 cleanse *)
let rec cleanse l = match l with
  | [] -> []
  | h::t ->
      let i = List.hd t and j = List.tl t in
      if h = i then h::(cleanse j)
      else h :: cleanse t;;

let v9 = cleanse [1;1;2;2;3;3];;