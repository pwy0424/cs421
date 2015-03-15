(*
 * File: mp7-skeleton.ml
   Team with: Yifang Zhang - zhang303
 *)

open Mp7common

(* Problem 1*)
let asMonoTy1 () = mk_fun_ty (bool_ty) (mk_list_ty int_ty)
let asMonoTy2 () = 
	let a = fresh() in
	let b = fresh() in
	let c = fresh() in
	let d = fresh() in
	mk_fun_ty a (mk_fun_ty b (mk_fun_ty c d)) 
let asMonoTy3 () = 
	let a = fresh() in
	let b = fresh() in
	mk_fun_ty a (mk_list_ty (mk_pair_ty b int_ty))
let asMonoTy4 () = 
	let a = fresh() in
	let b = fresh() in
	mk_pair_ty string_ty (mk_fun_ty (mk_list_ty b) a)

(* Problem 2*)
let rec subst_fun subst m = 
	match subst with
	| [] -> TyVar m
	| (a, b)::cs ->
		if a = m then b
		else subst_fun cs m 

(* Problem 3*)
let rec monoTy_lift_subst subst monoTy = 
	match monoTy with
	| TyVar m -> 
		subst_fun subst m 
	| TyConst(str, mono_list) -> 
		TyConst(str, List.map(monoTy_lift_subst subst) mono_list)

(* Problem 4*)
let rec occurs x ty = 
	match ty with
	| TyVar m -> (m = x)
	| TyConst(str, mono_list) -> 
		List.exists (occurs x) mono_list 

(* Problem 5*)
let rec unify eqlst =
	let rec add_new_equation l1 l2 x =
		match l1, l2 with
		| [], [] -> Some x
		| (a::bs, c::ds) -> add_new_equation bs ds ((a,c)::x)
		| _ -> None
	in
match eqlst with
| [] -> Some([])
| (s,t)::s' ->
	if s = t then unify s' 
		else 
		(
			match (s, t) with

			| (TyVar(m),t) -> 
				if (occurs m t) then None
				else let s'' = 
					List.map (fun (var1, var2) -> (monoTy_lift_subst [(m, t)] var1, monoTy_lift_subst [(m,t)] var2)) s'
	 			in 
				(
					let phi = unify s'' in
						match phi with 
						| None -> None
						| Some x -> Some((m, monoTy_lift_subst x t):: x)
				)

			| (TyConst(name, type_list), TyConst(name2, type_list2)) ->
				if name = name2 then 
				(
					match (add_new_equation type_list type_list2 s') with 
					| None -> None 
					| Some x -> unify x
				)
				else None

			| (TyConst(name, type_list), TyVar(m)) ->
				unify ((TyVar(m), TyConst(name, type_list))::s')
		)

(* Extra Credit *)

let equiv_types ty1 ty2 =
	let helper helper_ty =
		let rec go_first f l = 
			match l with 
			| [] -> None
			| (a :: bs) -> 
				if f a then Some a 
				else go_first f bs 
		in		
		let rec rec_canon sub m ty =
			match ty with 
			| TyVar n -> 
				(
					match go_first (fun p -> fst p = n) sub with 
					| None -> ((n,m)::sub), m+1, TyVar m
					| Some (l1,l2) -> (sub, m, TyVar l2)
				)
			
			| TyConst (c, tys) ->
				(
					match List.fold_left 
			  			(fun (sub, n, tyl) -> fun ty -> 
						  	(match rec_canon sub n ty with 
						  		| (sub', n', ty') -> 
					  			(sub', n', ty'::tyl)
					  		)
			  			) (sub, m, []) tys 
					with  
			  			(sub2, m2, tys2) -> 
			  				let rec reverse_list lst acc = 
			  					match lst with
			  						| [] -> []
			  						| ele::ele_rest -> reverse_list ele_rest acc@[ele]
			  				in
			  				(sub2, m2, TyConst(c, (reverse_list tys2 [])))
				)
		in 
		let (_, _, ty') = rec_canon [] 0 helper_ty 
		in ty'

	in
	let ty1' = helper ty1 in
	let ty2' = helper ty2 in 
	(ty1' = ty2')
