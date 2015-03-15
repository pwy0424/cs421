(* CS421 - Spring 2014
 * MP2 
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You will want to change how a number of these start.

Team with: zhang303

 *)


(*Problem 1*)
let dist (x1, y1) (x2, y2) = sqrt ((x1 -. x2) *. (x1 -. x2) +. (y1 -. y2) *. (y1 -. y2))

(*Problem 2*)
let rec fibo_num n = 
  if n <= 0 then 0
  else if n = 1 then 1
  else (fibo_num(n-1)+fibo_num(n-2)) 

(*Problem 3*)
let rec fibo_sum n = 
  if n = 1 then 1
  else (fibo_sum(n-1) + fibo_num(n))

(*Problem 4*)
let reverse_triple_to_list (a, b, c) = [c;b;a]

(*Problem 5*)
let rec sum l = 
  match l with []->0
  | (a :: bs)-> (a+(sum bs))

(*Problem 6*)
let rec min l default  =
  match l with [] -> default
  | [a]->a
  | (a :: bs)->
    let b = min bs default in
      if (a < b) then a
      else b

(*Problem 7*)
let rec is_sorted_ascend l =
  match l with [] -> true
  | [a] -> true
  | [a;b] -> (a <= b)
  | (a :: b :: cs) ->
    if (is_sorted_ascend (b::cs) = false) then false
    else (a <= b)

(*Problem 8*)
let rec zip l1 l2 =
  match l1 with []->[]
  | (a::bs) ->
    (match l2 with []->[]
    | (c::ds) -> (a,c):: zip bs ds)

(*Problem 9*)
let rec unzip l = 
  match l with []-> [],[]
  |(x,y)::s-> 
    let xs,ys = unzip s 
    in x::xs, y::ys;;

(*Problem 10*)
let rec add_odd_pos l = 
  match l with []->0
  |[a] -> a
  |(a::b::cs)-> (a + add_odd_pos(cs))

(*Problem 11*)
let rec insert n l =
  match l with []->[n]
  |(a::bs)->
    if(a > n) then n::a::bs
    else a::(insert n bs)

(*Problem 12*)
let rec is_prime a b = 
  if(b = 1) then true
  else (
    if(is_prime a (b-1) = false) then false
    else ((a mod b) != 0)
  )

let rec prime_next n = 
  if(is_prime (n+1) (n) = true) then (n+1)
  else prime_next(n+1)

let rec prime n =
  if(n = 1) then 2
  else prime_next(prime(n-1))

let rec primes n = 
  if(n <= 0) then []
  else (prime(n)::primes(n-1))
