open Mp3common
open List

(* Problem 1 *)
let rec even_count l =
	match l with [] -> 0 
		| (a::bs) -> 
		(let n = even_count bs in
				if(a mod 2 = 0) then (n+1)
				 else n
		)

(* Problem 2 *)
let rec split_sum l f = 
	match l with [] -> (0,0)
		| (a::bs) ->
		(let m,n = split_sum bs f in
			if(f a = true) then (a + m, n)
			else (m, n + a)
		)

(* Problem 3 *)
let rec merge l1 l2 = 
	match l1 with [] -> l2
		| (a::bs) -> 
			(match l2 with [] -> l1
				| (c::ds) -> 
					(
						let l3 = (merge bs ds) in
							(a::c::l3)
					)
			)

(* Problem 4 *)
let rec range_count l m n = 
	let rec range_count_helper l m n count = 
		match l with [] -> count
			| (a::bs) ->
				if (a > m)&&(a < n) then (range_count_helper bs m n (count+1))
				else (range_count_helper bs m n count)
	in range_count_helper l m n 0

(* Problem 5 *)
let rec max_index l = 
	let rec max_index_helper l max result count =
		match l with [] -> result
			|(a::bs) ->
				if(a > max) then max_index_helper bs a [count] (count+1)
				else if (a = max) then max_index_helper bs a (count::result) (count+1)
				else max_index_helper bs max result (count+1)
	in match l with [] -> []
		| (a::bs) -> max_index_helper l a [] 0 

(* Problem 6 *)
let rec reverse l result = 
	match l with [] -> result
		| (a::bs) -> reverse bs (a::result)

let rec unique_to_neighbor l = 
	let rec unique_to_neighbor_helper l result = 
		match l with [] -> result
			| [a] -> a::result
			| (a::b::cs) -> 
				if (a = b) then unique_to_neighbor_helper (b::cs) result
				else unique_to_neighbor_helper (b::cs) (a::result)
	in reverse (unique_to_neighbor_helper l []) []
		

(* Problem 7 *)
let remove_if_base = [] (* You may need to change this *)
let remove_if_rec p x r = 
	if (p x = false) then x::r
	else r

(* Problem 8 *)
let split_sum_base = (0,0) (* You may need to change this *)
let split_sum_rec f x r = 
	let n, m = r in 
		( if (f x = true) then (n + x), m
			else n, (m + x)
		)

(* Problem 9 *)
let all_positive_base = (true) (* You may need to change this *)
let all_positive_rec r x = 
	if (x > 0) then (true && r)
	else false

(* Problem 10 *)
let range_count_base = 0 (* You may need to change this *)
let range_count_rec m n r x = 
	if ((m < x) && (n > x)) then (r+1)
	else r

(* Problem 11 *)
let app_all_with fs b l  = 
	List.map (fun f -> List.map (fun g -> f b g) l) fs
(* got help from Chizheng Wang *)

(* Problem 12 *)
let reverse_ l = 
	List.fold_left (fun f x -> (x::f)) [] l;;

let rle l = 
	match l with [] -> []
		| (a::bs) -> rev 
			( List.fold_left 
				(fun g x ->
					match g with
						| ((c,d)::es) -> 
							if (x = c) then ((c,(d+1))::es)
							else ((x,1)::g)
				) 
				[(a,1)] bs
			)
	
