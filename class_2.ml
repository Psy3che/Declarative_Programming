(* ==============================================
   1. 幂运算相关函数
   ============================================== *)

(* 普通递归版幂运算：pow x y = x^y *)
let rec pow x y =
  if y = 0 then 1
  else x * (pow x (y - 1));;

(* 尾递归版幂运算（更高效）：pow_tr x y = x^y *)
let rec pow_tr x y =
  let rec powaux x y acc =
    if y = 0 then acc
    else powaux x (y - 1) (x * acc)
  in
  if y = 0 then 1
  else powaux x y 1;;

(* 另一种尾递归幂运算：用目标值 z 做对比，powTr x y = x^y *)
let rec powTr x y =
  let rec powAux x y z acc =
    if y = z then acc
    else powAux x y (z + 1) (x * acc)
  in
  powAux x y 0 1;;

(* 测试：powTr 2 4 = 16 *)


(* ==============================================
   2. 列表删除操作
   ============================================== *)

(* 删除列表中第一个出现的 5：del5 lst *)
let rec del5 lst = match lst with
  | [] -> []
  | h::t -> if h = 5 then t else h :: (del5 t);;

(* 删除列表中第一个出现的 x：delx lst x *)
let rec delx lst x = match lst with
  | [] -> []
  | h::t -> if h = x then t else h :: (delx t x);;

(* 偏函数：删除列表中第一个出现的 7 *)
let del7 lst = delx lst 7;;
let l' = del7 [1;5;5;2;7;5;8;7];;  (* 结果: [1;5;5;2;5;8;7] *)

(* 删除列表中所有的 5：delall5 lst *)
let rec delall5 lst = match lst with
  | [] -> []
  | h::t -> if h = 5 then delall5 t else h :: (delall5 t);;
let l'' = delall5 [1;5;5;2;7;5;8;7];;  (* 结果: [1;2;7;8;7] *)

(* 尾递归版删除所有 5：delall5_tr lst *)
let rec delall5_tr lst =
  let rec del5aux lst acc = match lst with
    | [] -> acc
    | h::t -> if h = 5 then del5aux t acc else del5aux t (h::acc)
  in
  let rec rev_tr l =  (* 内部尾递归反转 *)
    let rec revaux l acc = match l with
      | [] -> acc
      | h::t -> revaux t (h::acc)
    in revaux l []
  in
  rev_tr (del5aux lst []);;
let l''' = delall5_tr [1;5;5;2;7;5;8;7];;  (* 结果: [1;2;7;8;7] *)


(* ==============================================
   3. 列表反转
   ============================================== *)

(* 普通递归版反转：reverse lst *)
let rec reverse l = match l with
  | [] -> []
  | h::t -> reverse t @ [h];;

(* 尾递归版反转：rev_tr lst *)
let rec rev_tr l =
  let rec revaux l acc = match l with
    | [] -> acc
    | h::t -> revaux t (h::acc)
  in
  revaux l [];;
let l4 = rev_tr [1;5;5;2;7;5;8;7];;  (* 结果: [7;8;5;7;2;5;5;1] *)


(* ==============================================
   4. 列表拼接
   ============================================== *)

(* 列表拼接：myappend lst1 lst2 = lst1 @ lst2 *)
let rec myappend lst1 lst2 = match lst1 with
  | [] -> lst2
  | h::t -> h :: (myappend t lst2);;
let l5 = myappend [1;2;3] [4;5;6];;  (* 结果: [1;2;3;4;5;6] *)


(* ==============================================
   5. 列表操作：尾部、最后一个元素
   ============================================== *)

(* 获取列表尾部：tail lst *)
let tail lst = match lst with
  | _::t -> t;;

(* 获取列表最后一个元素：lastlst lst，空列表抛出异常 *)
let rec lastlst l = match l with
  | [] -> failwith "empty list"
  | [x] -> x
  | _::t -> lastlst t;;
let v1 = lastlst [1;2;3;4;5];;  (* 结果: 5 *)


(* ==============================================
   6. 素数与质因数分解
   ============================================== *)

(* 判断 n 是否能被 p 整除：divide n p *)
let divide n p = (n mod p) = 0;;

(* 判断素数：prime' n，试除法到 sqrt(n) *)
let prime' n =
  let rec aux n p =
    if p * p > n then true
    else (not (divide n p)) && aux n (p + 1)
  in
  if n < 2 then false
  else aux n 2;;

let p3 = divide 121 11;;   (* true *)
let p4 = prime' 101;;      (* true *)
let p5 = prime' 105;;      (* false *)
let p6 = prime' 169;;      (* false (13²) *)
let p7 = prime' 201;;      (* false (3×67) *)

(* 质因数分解：decompose n，返回质因数列表（倒序）
   例：decompose 20 = [5;2;2] *)
let rec decompose n =
  let rec aux n p list =
    if p * p > n then list
    else if divide n p then
      let q = n / p in aux q p (p::list)
    else aux n (p + 1) list
  in
  if n = 1 then []
  else aux n 2 [];;

let v4 = decompose 201;;  (* [67; 3] *)