(* File: mp5-skeleton.ml *)
(* Team with: Qian Sun - qiansun1 and Yifang Zhang - zhang303 *)

open Mp5common

(* Problem 1 *)
let rec import_list lst =
	match lst with
		| [] -> ConstExp(NilConst)	
		| ((a, b) :: cs) -> 
			BinOpAppExp(ConsOp, 
				BinOpAppExp(CommaOp, ConstExp(StringConst a), ConstExp(IntConst b)),
				import_list cs
			)

(* Problem 2 *)
let list_all = 
	Rec("list_all",  "p",
		FnExp("xs",
			IfExp(BinOpAppExp(EqOp, VarExp("xs"), ConstExp(NilConst)),
				ConstExp(BoolConst(true)),
					IfExp(AppExp(VarExp("p"),MonOpAppExp(HdOp,VarExp("xs"))),
						IfExp(
							AppExp(
								AppExp(VarExp("list_all"),VarExp("p")),
								MonOpAppExp(TlOp,VarExp("xs"))
							),
							ConstExp(BoolConst(true)),
							ConstExp(BoolConst(false))
						),
					ConstExp(BoolConst(false))
				)  			
			)	
		) 
	)


(* Problem 3 *)
let rec cal_max_exp_height exp =
	match exp with
		| VarExp(_) | ConstExp(_) -> 1
		| MonOpAppExp(_, a)	| FnExp(_, a) -> (cal_max_exp_height a) + 1
		| BinOpAppExp(_, a, b) | AppExp(a, b) -> 
			(max (cal_max_exp_height a) (cal_max_exp_height b)) + 1
		| IfExp(a, b, c) -> 
				(max 
					(max (cal_max_exp_height a) (cal_max_exp_height b)) 
					(cal_max_exp_height c)
				) + 1
		 | LetExp(a, b) -> 
			(max (cal_max_dec_height a) (cal_max_exp_height b))+1

and cal_max_dec_height dec = 
	match dec with
		| Rec(_, _, a) | Val(_, a) -> 
			(cal_max_exp_height a) + 1
		| Seq(a, b) -> 
			(max (cal_max_dec_height a) (cal_max_dec_height b)) + 1

(* Problem 4 *)
let rec freeVarsInExp exp = 
	match exp with
		| ConstExp(_) -> []
		| VarExp(a) -> [a] 
		| BinOpAppExp(_, a, b) | AppExp(a, b) -> 
			(freeVarsInExp a) @ (freeVarsInExp b)
		| MonOpAppExp(_, a) -> 
			(freeVarsInExp a)
		| FnExp(a, b) -> 
			List.filter (fun f -> not (f = a)) (freeVarsInExp b)
		| LetExp(a, b) -> 
			let (c, d) = freeAndBindingVarsInDec a
			in (c @ (List.filter (fun f -> not(List.mem f d)) (freeVarsInExp b)))
		| IfExp(a, b, c) -> 
			((freeVarsInExp a) @ (freeVarsInExp b)) @ (freeVarsInExp c)

and freeAndBindingVarsInDec dec = 
	match dec with
		| Val(a, b) -> (freeVarsInExp b, [a])
		| Rec(a, b, c) -> 
			(List.filter (fun f -> not(List.mem f [a;b])) (freeVarsInExp c),[a])
		| Seq(a, b) -> 
			let (a1, b1) = freeAndBindingVarsInDec a
			in
				let (a2, b2) = freeAndBindingVarsInDec b
				in
					(a1 @ (List.filter (fun f -> not(List.mem f b1)) a2), b1 @ b2);;

(* Problem 5 *)
let rec cps_exp e k kx =  
	match e with
		| VarExp(v) -> (VarCPS(k, v), kx)
		| ConstExp(c) -> (ConstCPS(k, c), kx)
		| IfExp(a, b, c) -> 
			let (d, e) = cps_exp b k kx 
			in
				let (d2, e2) = cps_exp c k e
				in
					let f = 
						freshFor ((freeVarsInExp b) @ (freeVarsInExp c) @ (freeVarsInContCPS k)) 
					in
						(cps_exp a (ContCPS(f, IfCPS(f, d, d2))) e2)
	| MonOpAppExp(a, b) -> 
		let c = freshFor ((freeVarsInExp b) @ (freeVarsInContCPS k)) 
		in
			(cps_exp b (ContCPS(c, MonOpAppCPS(k, a, c))) kx)
	| BinOpAppExp(a, b, c) -> 
		let d = freshFor ((freeVarsInExp b) @ (freeVarsInExp c) @ (freeVarsInContCPS k)) 
		in
			let e = 
				freshFor ((freeVarsInExp b) @ (freeVarsInExp c) @ (freeVarsInContCPS k) @ [d]) 
			in
				let (f, g) = cps_exp c (ContCPS(e, BinOpAppCPS(k, a, d, e))) kx 
				in
					(cps_exp b (ContCPS(d, f)) g)
	| AppExp(a, b) -> 
		let c = freshFor ((freeVarsInExp a) @ (freeVarsInContCPS k)) 
		in
			let d = freshFor ((freeVarsInContCPS k) @ [c]) 
			in
				let (e, f) = cps_exp b (ContCPS(d, AppCPS(k, c, d))) kx 
				in
					(cps_exp a (ContCPS(c,e)) f)
	| FnExp(a, b) -> 
		let (c, d) = cps_exp b (ContVarCPS kx) (kx + 1) 
		in
			(FnCPS(k, a, kx, c), d)
	| LetExp(a, b) -> 
		let (c, d) = cps_exp b k kx 
		in
			(cps_dec a c d)

and cps_dec dec ecps kx =  
	match dec with
		| Val(a, b) -> 
			cps_exp b (ContCPS(a, ecps)) kx		
		| Seq(a, b) -> 
			let (c, d) = cps_dec b ecps kx 
			in
				cps_dec a c d
		| Rec(a, b, c) -> 
			let (d, e) = cps_exp c (ContVarCPS kx) (kx + 1) 
			in
				(FixCPS(ContCPS(a, ecps), a, b, kx, d), e)



