open Mp4common
(* team up with Yifang Zhang - zhang303
   	     and Qian Sun - qiansun1 *)


(* Problem 1 *)
let addk n m k =
  k(n+m)
let subk n m k =
  k(n-m)
let mulk n m k =
  k(n*m)
let posk x k =
  k(x>0)
let float_addk a b k =
  k(a+.b)
let float_divk a b k =
  k(a/.b)
let catk str1 str2 k =
  k(str1^str2)
let consk e l k =
  k(e::l)
let eqk x y k =
  k(x = y)
let geqk x y k =
  k(x >= y)

(* Problem 2 *)
let poly x k =
	mulk x x 
		(fun f -> 
			mulk f x 
				(fun fplusX -> 
					addk fplusX x 
						(fun fplusXplus1 -> 
							addk fplusXplus1 1 k
						)	                  
				)
		)

(* Problem 3 *)
let composek f g x k = 
  f x (fun fx -> g fx k)

(* Problem 4 *)
let rec inverse_square_series n = 
  if(n<=0) then 0.0
  else (1.0/.((float)(n * n))) +. inverse_square_series (n-1)

let float_of_intf n f = f (float_of_int n)

let rec inverse_square_seriesk n k= 
  posk n 
    (fun f ->
			if f then mulk n n 
				(fun n_times_n -> 
					float_of_intf n_times_n
						(fun float_n_times_n ->
							float_divk 1.0 float_n_times_n
								(fun floatdiv ->
									inverse_square_seriesk (n-1)
										(fun remainder ->
											float_addk remainder floatdiv k
										)			
								)
						)
				)
			else k 0.0
    )

(* Problem 5 *)
let rec rev_map f l =
  match l with [] -> []
		| (a::bs) -> (let r = (rev_map f bs) in (f a) :: r)

let rec rev_mapk f l k = 
   match l with []-> k []
		| (a::bs) -> 
			rev_mapk f bs 
				(fun remainder -> 
					f a 
						(fun g -> 
							consk g remainder k
						)
				)

(* Problem 6 *)
let rec partition l p = 
	match l with [] -> ([], [])
		| (a::bs) -> 
			let x, y = partition bs p in 
				if p a = true then (a::x, y)
				else (x, a::y)

let rec partitionk l p k =  
	match l with [] -> k ([], [])
		| (a::bs) -> 
			partitionk bs p
				(fun (f, g) -> 
						p a 
							(fun h -> if h then 
								consk a f (fun fx -> k (fx, g))
					    else
								consk a g (fun gx -> k (f, gx))
							)
				)

(* Problem 7 *)
let rec findk l p normalk exceptionk = 
	match l with [] -> exceptionk()
		| (a::bs) -> p a
			(fun f ->
				if f then normalk a
				else findk bs p normalk exceptionk
			)

(* Problem 8 *)
let rec appk l x k =
	match l with []-> k x
		| (a::bs) -> 
			appk bs x
				(fun remainder ->
					a remainder k
				)

