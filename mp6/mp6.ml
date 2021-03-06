open Mp6common
(* Team with: Yifang Zhang - zhang303 *)

let rec gather_exp_ty_substitution gamma exp tau =
    let judgment = ExpJudgment(gamma, exp, tau) in
    match exp
    with ConstExp c ->
         let tau' = const_signature c in
         (match unify [(tau, freshInstance tau')]
          with None       -> None
             | Some sigma -> Some(Proof([],judgment), sigma))
	 
	| VarExp v ->
		let env_v = lookup_env gamma v in
			(match env_v with None -> None
				| Some tau' -> 
					(match unify [(tau, freshInstance tau')] with None -> None
						| Some sigma -> Some(Proof([],judgment), sigma)
					)
			)

	| BinOpAppExp (bop, e1, e2) ->
		let tau1 = fresh() in
			(let op1 = gather_exp_ty_substitution gamma e1 tau1 in
				(match op1 with None -> None
					| Some (proof1, sigma1) -> 
						let tau2 = fresh() in
							(let op2 = gather_exp_ty_substitution (env_lift_subst sigma1 gamma) e2 tau2 in
								(match op2 with None -> None
									| Some (proof2, sigma2) -> 
										let tau' = binop_signature bop in
											(let env = monoTy_lift_subst (subst_compose sigma2 sigma1) (mk_fun_ty tau1 (mk_fun_ty tau2 tau)) in
												(let result = unify [env, freshInstance tau'] in
													(match unify [env, freshInstance tau'] with None -> None
														| Some sigma -> Some(Proof([proof1;proof2], judgment), subst_compose sigma (subst_compose sigma2 sigma1))
													)
												)
											)
								)
							)
				)
			)
  
	| MonOpAppExp (mop, e1) ->
		let tau1 = fresh() in
			(let op1 = gather_exp_ty_substitution gamma e1 tau1 in
				match op1 with None -> None
					| Some (proof1, sigma1) ->
						let tau' = monop_signature mop in
							(let result = unify [(monoTy_lift_subst sigma1 (mk_fun_ty tau1 tau)), freshInstance tau'] in
								(match result with None -> None
									| Some(sigma) -> Some(Proof([proof1], judgment), subst_compose sigma sigma1)
								)
							)
			)

	| IfExp (e1, e2, e3) ->
		let tau1 = bool_ty in
			(let op1 = gather_exp_ty_substitution gamma e1 tau1 in
				(match op1 with None -> None
					| Some(proof1, sigma1) ->
						let op2 = (gather_exp_ty_substitution (env_lift_subst sigma1 gamma) e2 (monoTy_lift_subst sigma1 tau)) in
							(match op2 with None -> None
								| Some(proof2, sigma2) ->
									let sum = subst_compose sigma2 sigma1 in
										(let result = gather_exp_ty_substitution (env_lift_subst sum gamma) e3 (monoTy_lift_subst sum tau) in
											(match result with None -> None
												| Some(proof3, sigma3) ->
													Some(Proof([proof1;proof2;proof3], judgment), (subst_compose sigma3 sum) )
											)
										)
							)
				)
			)

	| FnExp (s, e1) ->
		let tau1 = fresh() in
			(let tau2 = fresh() in
				(let op = gather_exp_ty_substitution (ins_env gamma s (polyTy_of_monoTy tau1)) e1 tau2 in
					(match op with None -> None
						| Some(proof1, sigma1) ->
							let result = unify [(monoTy_lift_subst sigma1 tau), monoTy_lift_subst sigma1 (mk_fun_ty tau1 tau2)] in
								(match result with None -> None
									| Some(sigma) -> Some(Proof([proof1], judgment), (subst_compose sigma sigma1)) 
								)
					)
				)
			)

	| RaiseExp (e) ->
		let result = gather_exp_ty_substitution (gamma) e int_ty in
		(match result with None -> None
			| Some(proof1, sigma1) -> 
				Some(Proof([proof1], judgment), sigma1)
		)

	| AppExp (e1, e2) ->
		let tau1 = fresh() in
			(let op = (gather_exp_ty_substitution (gamma) e1 (mk_fun_ty tau1 tau)) in
				(match op with None -> None
					| Some(proof1, sigma1) ->
						let result = (gather_exp_ty_substitution (env_lift_subst sigma1 gamma) e2 (monoTy_lift_subst sigma1 tau1)) in
							(match result with None -> None
								| Some(proof2, sigma2) -> 
									Some(Proof([proof1;proof2], judgment), subst_compose sigma2 sigma1 )
							)		
				)
			)

	| LetExp(d,e) -> 
	 	let op = gather_dec_ty_substitution gamma d in
			(match op with None -> None
				| Some(proof1, delta, sigma1) ->
					let result = gather_exp_ty_substitution (sum_env delta (env_lift_subst sigma1 gamma)) e (monoTy_lift_subst sigma1 tau) in
						(match result with None -> None
							| Some(proof2, sigma2) -> 
								Some(Proof([proof1;proof2], judgment), subst_compose sigma1 sigma2)
						)
			)

	| HandleExp (e,_,e1,nelist) ->
		let op1 = gather_exp_ty_substitution gamma e tau in
			(match op1 with None -> None
				| Some(proof, sigma) -> 
					let result = gather_exp_ty_substitution (env_lift_subst sigma gamma) e1 (monoTy_lift_subst sigma tau) in
						(match result with None -> None
							| Some(proof1,sigma1) -> 
								let rec gather_list exp proofn sigman =
									(match exp with [] -> Some(proofn,sigman)
										| (_,e)::nes -> 
											let resultn = gather_exp_ty_substitution (env_lift_subst sigman gamma) e (monoTy_lift_subst sigman tau) in
												(match resultn with None -> None
													| Some(proof_t,sigma_t) -> 
														gather_list nes (proofn@[proof_t]) (subst_compose sigma_t sigman)
												)
									) 
								in
					       			(let result_t = gather_list nelist ([proof;proof1]) (subst_compose sigma1 sigma) in
				       					(match result_t with None -> None
											| Some(proofn,sigman) -> 
												Some(Proof(proofn, judgment), sigman)
										)
									)
						)
			)


and gather_dec_ty_substitution gamma dec = 
   match dec with
	| Val(x, e) -> 
		let tau = fresh() in
			(match gather_exp_ty_substitution gamma e tau with None -> None
				| Some(proof1, sigma1) ->
					let env = make_env x (gen (env_lift_subst sigma1 gamma) (monoTy_lift_subst sigma1 tau)) in
						Some(Proof([proof1],DecJudgment(gamma, dec, env)), env, sigma1)
			)
	
	| Rec(f, x, e) ->
		let tau1 = fresh() in
			(let tau2 = fresh() in
				(let gamma1 = ins_env (ins_env gamma x (polyTy_of_monoTy tau1)) f (polyTy_of_monoTy (mk_fun_ty tau1 tau2)) in
					(let result = gather_exp_ty_substitution gamma1 e tau2 in
						(match result with None -> None
							| Some(proof1, sigma1) -> 
								let env = make_env f (gen (env_lift_subst sigma1 gamma) (monoTy_lift_subst sigma1 (mk_fun_ty tau1 tau2))) in 
									Some(Proof([proof1], DecJudgment(gamma, dec, env)), env, sigma1)
						)
					)
				)
			)
		
	| Seq(d1, d2) -> 
		(match gather_dec_ty_substitution gamma d1 with None -> None
			| Some(proof1, delta1, sigma1) -> 
				let result = gather_dec_ty_substitution (env_lift_subst sigma1 (sum_env delta1 gamma)) d2 in
					(match result with None -> None
						| Some(proof2, delta2, sigma2) -> 
							let sum = subst_compose sigma2 sigma1 in
								(let env = env_lift_subst sum (sum_env delta2 delta1) in
									Some(Proof([proof1;proof2], DecJudgment(gamma, dec, env)), env, sum)
								)
					)
		)

    | Local(d1, d2) -> 
		let op1 = gather_dec_ty_substitution gamma d1 in
			(match op1 with None -> None
				| Some(proof1, delta1, sigma1) -> 
					let op2 = gather_dec_ty_substitution (env_lift_subst sigma1 (sum_env delta1 gamma)) d2 in
						(match op2 with None -> None
							| Some(proof2, delta2, sigma2) -> 
								let sum = subst_compose sigma2 sigma1 in
									let env = env_lift_subst sum delta2 in
										Some(Proof([proof1;proof2], DecJudgment(gamma, dec, env)), env, sum)
						)
			)
