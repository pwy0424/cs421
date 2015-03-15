open Mp10common;;

(* Team with: Yifang Zhang - zhang303 *)
(* get help from furquans *)

let const_to_val c = 
	match c with 
		| BoolConst b -> BoolVal b
		| IntConst i -> IntVal i
		| RealConst r -> RealVal r
		| StringConst s -> StringVal s
		| NilConst -> ListVal []
		| UnitConst -> UnitVal

let monOpApply unop v = 
	match (unop,v) with
		| (IntNegOp, IntVal i) -> IntVal(-i)
		| (HdOp, ListVal lst) -> 
			(
				match lst with 
					| [] -> Exn(0)
					| x::xs -> x
			)
		| (TlOp, ListVal lst) ->
      		(
				match lst with 
					| [] -> Exn(0)
					| x::xs -> ListVal(xs) 
			)
		| (FstOp, PairVal(v1,v2)) -> v1
		| (SndOp, PairVal(v1,v2)) -> v2
		| (PrintStringOp, StringVal s) -> print_string s; UnitVal

let binOpApply binop v1 v2 =
	match (binop,v1,v2) with
		| (IntPlusOp, IntVal i1, IntVal i2) -> IntVal(i1+i2)
		| (IntMinusOp, IntVal i1, IntVal i2) -> IntVal(i1-i2)
		| (IntTimesOp, IntVal i1, IntVal i2) -> IntVal(i1*i2)
		| (IntDivOp, IntVal i1, IntVal i2) -> if i2 != 0 then IntVal(i1/i2) else Exn(0)
		| (RealPlusOp, RealVal r1, RealVal r2) -> RealVal(r1+.r2)
		| (RealMinusOp, RealVal r1, RealVal r2) -> RealVal(r1-.r2)
		| (RealTimesOp, RealVal r1, RealVal r2) -> RealVal(r1*.r2)
		| (RealDivOp, RealVal r1, RealVal r2) -> if r2 != 0.0 then RealVal(r1/.r2) else Exn(0)
		| (ConcatOp, StringVal s1, StringVal s2) -> StringVal (s1^s2)
		| (ConsOp, a, ListVal bs) -> ListVal(a::bs)
		| (CommaOp, v1, v2) -> PairVal(v1,v2)
		| (EqOp, v1, v2) -> BoolVal(v1 = v2)
		| (GreaterOp, v1, v2) -> BoolVal(v1>v2)


let rec get_value_list key l =
	match l with
		|  [] -> None
		| ((Some value,e)::bs) -> if value = key then Some e else get_value_list key bs
		| ((None,e)::bs) -> Some e

let rec eval_exp (exp, m) = 
	match exp with
		| ConstExp c -> const_to_val c
		| FnExp(x,e) -> ClosureVal(x,e,m)
		| VarExp e -> 
			(
				match lookup_env m e with 
					| Some RecVarVal(f, x, e', m') -> ClosureVal (x, e', ins_env m x (RecVarVal(f, x, e', m')))
					| Some v -> v
			)
		| AppExp(e1,e2) ->
			(
				match eval_exp (e1,m) with
					|Exn(i) -> Exn(i)
					| ClosureVal(x,e',m') -> 
						(
							match eval_exp (e2,m) with
								| Exn(i) -> Exn(i)
								| v' -> eval_exp (e',(ins_env m' x v'))
						)
			)
		| MonOpAppExp(unop,e) ->
			(
				match eval_exp (e,m) with
					| Exn(i) -> Exn(i)
					| v -> monOpApply unop v
			)
		| BinOpAppExp(binop,e1,e2) -> 
			(
				match eval_exp (e1,m) with
					| Exn(i) -> Exn(i)
					| v1 -> 
						(
							match eval_exp (e2,m) with
								| Exn(j) -> Exn(j)
								| v2 -> binOpApply binop v1 v2
						)
			)
		| IfExp(e1,e2,e3) ->
			(
				match eval_exp (e1,m) with
					| BoolVal(true) -> eval_exp (e2,m)
					| BoolVal(false) -> eval_exp (e3,m)
					| Exn(i) -> Exn(i)
			)
		| LetExp(d,e) ->
			(
				match eval_dec (d,m) with
	  				| ((None,Exn(i))::tl,m') -> Exn(i)
					| (b,m') -> eval_exp (e,(sum_env m' m))
			)
		| RaiseExp(e) ->
			(
				match eval_exp (e,m) with
					|Exn(i) -> Exn(i)
					| IntVal(v) -> Exn(v)
			)
		| HandleExp(e,int_opt,e0,lst) ->
			(
				match eval_exp (e,m) with
					| Exn(j) -> 
						(
							match get_value_list j ((int_opt,e0)::lst) with
								| None -> Exn(j)
								| Some ei -> eval_exp (ei,m)
						)
					| v -> v
			)

and eval_dec (dec, m) = 
	match dec with
		| Val(x,e) -> 
			(
				match eval_exp (e,m) with
					| Exn(i) -> ([(None,Exn(i))],empty_env)
					| v -> 
						if x = "" then ([(None,v)],empty_env)
						else ([(Some x,v)],(make_env x v))
			)
		| Seq(d1,d2) -> 
			(
				match eval_dec (d1,m) with
					| ((None,Exn(i))::tl,m') -> ((None,Exn(i))::tl,m')
					| (b1,m') ->
						(
							match eval_dec (d2, (sum_env m' m)) with
								| ((None,Exn(i))::tl,m') -> ((None,Exn(i))::tl,m')
								| (b2,m'') -> (b2@b1, sum_env m'' m')
						)
			)
		| Local(d1,d2) -> 
			(
				match eval_dec (d1,m) with
					| ((None,Exn(i))::tl,m') -> ((None,Exn(i))::tl,empty_env)
					| (b1,m') -> eval_dec (d2, (sum_env m' m))
			)
		| Rec(f,x,e) -> ([(Some f, RecVarVal(f,x,e,m))], (make_env f (RecVarVal(f,x,e,m))))

