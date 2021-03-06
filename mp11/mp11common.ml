(* File: mp11common.ml *)

(* Some general purpose functions *)

let set_minus a b = List.filter (fun x -> not (List.mem x b)) a

(* expressions for MicroML *)

type const =
     BoolConst of bool        (* for true and false *)
   | IntConst of int          (* 0,1,2, ... *)
   | RealConst of float       (* 2.1, 3.0, 5.975, ... *)
   | StringConst of string    (* "a", "hi there", ... *)
   | NilConst                 (* [ ] *)
   | UnitConst                (* ( ) *)

let string_of_const = function
   BoolConst b     -> (if b then "true" else "false")
 | IntConst i      -> string_of_int i
 | RealConst f     -> ((string_of_float f)^(if ceil f = floor f then ("0") else ("")))
 | StringConst s   -> ("\""^ (String.escaped s)^ "\"")
 | NilConst        -> "[]"
 | UnitConst       -> "()"

type mon_op =
     IntNegOp      (* integer negation *)
   | HdOp          (* hd *)
   | TlOp          (* tl *)
   | FstOp         (* fst *)
   | SndOp         (* snd *)
   | PrintStringOp (* print_string *)

let string_of_mon_op = function
     IntNegOp -> "~"
   | HdOp -> "hd"
   | TlOp -> "tl"
   | FstOp -> "fst"
   | SndOp -> "snd"
   | PrintStringOp -> "print_string"

type bin_op =
     IntPlusOp        (* _ + _ *)
   | IntMinusOp       (* _ - _ *)
   | IntTimesOp       (* _ * _ *)
   | IntDivOp         (* _ / _ *)
   | RealPlusOp       (* _ +. _ *)
   | RealMinusOp      (* _ -. _ *)
   | RealTimesOp      (* _ *. _ *)
   | RealDivOp        (* _ /. _ *)
   | ConcatOp         (* _ ^ _ *)
   | ConsOp           (* _ :: _ *)
   | CommaOp          (* _ , _ *)
   | EqOp             (* _ = _ *)
   | GreaterOp        (* _ > _ *)

let string_of_bin_op = function
     IntPlusOp  -> "+"
   | IntMinusOp -> "-"
   | IntTimesOp -> "*"
   | IntDivOp -> "/"
   | RealPlusOp -> "+."
   | RealMinusOp -> "-."
   | RealTimesOp -> "*."
   | RealDivOp -> "/."
   | ConcatOp -> "^"
   | ConsOp -> "::"
   | CommaOp -> ","
   | EqOp  -> "="
   | GreaterOp -> ">"

type dec =
     Val of string * exp             (* val x = exp *)
   | Rec of string * string * exp    (* val rec f x = exp *)
   | Seq of dec * dec                (* dec1 dec2 *)
   | Local of dec * dec              (* local dec1 in dec2 end *)

and exp =
   | VarExp of string                    (* variables *)
   | ConstExp of const                   (* constants *)
   | MonOpAppExp of mon_op * exp         (* % exp1
                    where % is a builtin monadic operator *) 
   | BinOpAppExp of bin_op * exp * exp   (* exp1 % exp2                                  
                    where % is a builtin binary operator *)
   | IfExp of exp * exp * exp            (* if exp1 then exp2 else exp3 *)
   | AppExp of exp * exp                 (* exp1 exp2 *) 
   | FnExp of string * exp               (* fn x => x *)
   | LetExp of dec * exp                 (* let dec in exp end *)
   | RaiseExp of exp                     (* raise e *)
   | HandleExp of (exp * int option * exp * (int option * exp) list)
                          (* e handle i => e0 | j => e1 | ...| k => en *)

let rec string_of_exp = function
   VarExp s -> s
 | ConstExp c ->  string_of_const c
 | IfExp(e1,e2,e3)->"if " ^ (string_of_exp e1) ^
                 " then " ^ (string_of_exp e2) ^
                 " else " ^ (string_of_exp e3)
 | MonOpAppExp (m,e) ->  (string_of_mon_op m) ^ " " ^ (paren_string_of_exp e) 
 | BinOpAppExp (b,e1,e2) -> 
   (match b with CommaOp -> ("(" ^ (paren_string_of_exp e1) ^ (string_of_bin_op b) ^
                              (paren_string_of_exp e2) ^ ")")
    | _ -> ((paren_string_of_exp e1) ^ " " ^ (string_of_bin_op b)
            ^ " " ^ (paren_string_of_exp e2)))
 | AppExp(e1,e2) -> (non_app_paren_string_of_exp e1) ^ " " ^ (paren_string_of_exp e2) 
 | FnExp (x,e) -> ("fn " ^ x ^ " => " ^ (string_of_exp e))
 | LetExp (d,e2) -> ("let " ^ (string_of_dec d) ^ " in " ^ (string_of_exp e2) ^ " end")
 | RaiseExp e -> "raise " ^ (string_of_exp e)
 | HandleExp (e,intopt1,exp1, match_list) ->
    (paren_string_of_exp e) ^  " handle " ^
     (string_of_exn_match (intopt1,exp1)) ^
     (List.fold_left (fun s m -> (s^" | " ^ (string_of_exn_match m))) "" match_list)

and string_of_dec = function
   Val (s, e) ->  ("val "^ (if s = "" then "_" else s) ^" = " ^ (string_of_exp e))
 | Rec (fname,argname,fn) ->
       ("val rec " ^ fname ^ " " ^ argname ^ " = " ^ (string_of_exp fn))
 | Seq (d1,d2) -> (string_of_dec d1) ^ "\n" ^ string_of_dec d2
 | Local (d1,d2) -> "local "^ (string_of_dec d1) ^
   "\nin " ^ (string_of_dec d2) ^ " end"

and paren_string_of_exp e =
    match e with VarExp _ | ConstExp _ -> string_of_exp e
    | _ -> "(" ^ string_of_exp e ^ ")"

and non_app_paren_string_of_exp e =
    match e with AppExp (_,_) -> string_of_exp e
    | _ -> paren_string_of_exp e

and string_of_exn_match (int_opt, e) =
    (match int_opt with None -> "_" | Some n -> string_of_int n) ^
    " => " ^
    (string_of_exp e)

let print_exp exp = print_string (string_of_exp exp)
let print_dec dec = print_string (string_of_dec dec)

let rec freeVarsInExp exp =
    match exp with VarExp x -> [x]
    | ConstExp c -> []
    | MonOpAppExp (m,e) -> freeVarsInExp e
    | BinOpAppExp (b,e1,e2) -> freeVarsInExp e1 @ freeVarsInExp e2
    | IfExp (e1,e2,e3) -> freeVarsInExp e1 @ (freeVarsInExp e2 @ freeVarsInExp e3)
    | AppExp (e1,e2) -> freeVarsInExp e1 @ freeVarsInExp e2
    | FnExp (f,e) -> set_minus (freeVarsInExp e) [f]
    | LetExp (d,e) -> 
      let (fvs, bvs) = freeAndBindingVarsInDec d
      in fvs @ (set_minus  (freeVarsInExp e) bvs)
    | HandleExp (e, n0, e0, rem_match) ->
      (freeVarsInExp e) @ (freeVarsInExp e0) @
      (List.fold_right (fun (n,exp) -> fun fv -> freeVarsInExp exp @ fv) rem_match [])
    | RaiseExp e -> freeVarsInExp e
      
and freeAndBindingVarsInDec dec = (*raise (Failure "Not implemented yet")*)
    match dec with Val (x,e) -> (freeVarsInExp e, [x])
    | Rec (f, x, e) -> (set_minus  (freeVarsInExp e) [f;x], [f])
    | Seq (d1,d2) ->
      let (fvs, bvs) = freeAndBindingVarsInDec d1 in
      let (fvs', bvs') =  freeAndBindingVarsInDec d2
      in (fvs @ (set_minus fvs' bvs), bvs@bvs')
    | Local (d1, d2) ->
      let (fvs, bvs) = freeAndBindingVarsInDec d1 in
      let (fvs', bvs') =  freeAndBindingVarsInDec d2
      in (fvs @ (set_minus fvs' bvs), bvs')


(* CPS and CPS transformation *)
 (* Not needed in mp6, mp7, mp8, mp9, or mp10 *)
type cps_cont = 
   ContVarCPS of int                          (* _ki *)
 | External
 | ContCPS of string * exp_cps                (* funk x --> exp_cps *)
 | ExnMatch of exn_cont                       (* i1 |-> ec1; ... in |-> ecn *)

and exn_cont =
   ExnContVarCPS of int
 | EmptyExnContCPS
 | UpdateExnContCSP of (int option * exp_cps) list * exn_cont

and exp_cps =
   VarCPS of cps_cont * string
 | ConstCPS of cps_cont * const
 | MonOpAppCPS of cps_cont * mon_op * string * exn_cont
 | BinOpAppCPS of cps_cont * bin_op * string * string * exn_cont
 | IfCPS of string * exp_cps * exp_cps
 | AppCPS of cps_cont * string * string * exn_cont
 | FnCPS of cps_cont * string * int * int * exp_cps
 | FixCPS of cps_cont * string * string * int * int * exp_cps 

(* CPS Free Vars functions *)
(*
let rec freeVarsInExpCPS cont =
    match cont with VarCPS (k, x) -> x :: freeVarsInContCPS k
    | ConstCPS (k, c) -> freeVarsInContCPS k
    | MonOpAppCPS (k,m,s,ke) -> s :: (freeVarsInContCPS k)@(freeVarsInExnConstCPS ke)
    | BinOpAppCPS (k,b,r,s,ke) -> r :: s :: (freeVarsInContCPS k)@(freeVarsInExnConstCPS ke)
    | IfCPS (r,e1,e2) -> r :: ((freeVarsInExpCPS e1) @ (freeVarsInExpCPS e2))
    | AppCPS (k,x1,x2,ke) -> x1::x2::(freeVarsInContCPS k)@(freeVarsInExnConstCPS ke)
    | FnCPS (k,x,c,ec,e) ->
      (freeVarsInContCPS k) @ (set_minus (freeVarsInExpCPS e) [x])
    | FixCPS (k,f,x,kx,ky,e) -> (freeVarsInContCPS k) @ (set_minus (freeVarsInExpCPS e) [f;x])

and
   freeVarsInContCPS k =
   match k with ContVarCPS c -> []
   | ClosedCPSCont c -> freeVarsInClosedContCPS c

and freeVarsInClosedContCPS k =
   match k  with External -> []
   | ContCPS (k, e) -> (freeVarsInExpCPS e)
   | ExnMatch ke -> freeVarsInExnConstCPS ke

and freeVarsInExnConstCPS ke =
   match ke with ExnContVarCPS n -> []
 | ClosedExnCont c -> freeVarsInClosedExnConstCPS c
 | UpdateExnContCSP (l,ke') ->
         List.fold_right
         (fun (n,ecps) -> fun fvs -> (freeVarsInExpCPS ecps) @ fvs)
         l (freeVarsInExnConstCPS ke')

and freeVarsInClosedExnConstCPS ke =
   match ke with UpdateClosedExnContCSP (l,ke') ->
         List.fold_right
         (fun (n,ecps) -> fun fvs -> (freeVarsInExpCPS ecps) @ fvs)
         l (freeVarsInClosedExnConstCPS ke')
   | EmptyExnContCPS -> [] 
*)
(* CPS: Fresh Name stuff *)

let (freshIntName , resetIntNameInt) =
    let intNameInt = ref 0 in
    let n() = (let x = !intNameInt in intNameInt := x + 1; string_of_int x) in
    let r() = intNameInt := 0 in
    (n,r)
(*
let int_to_string n =
    let int_to_int_26_list n =
        let rec aux n l =
            if n <= 0 then l else let c = ((n-1) mod 26) in aux ((n -(c+1))/26) (c::l)
        in aux n []
    in
        let rec aux l = match l with [] -> ""
                            | n::ns -> (String.make 1 (Char.chr (n + 97))) ^ aux ns
        in aux (int_to_int_26_list n)

let int_to_string = string_of_int

let freshFor lst = 
    let rec fresh_name n = 
        if List.mem (int_to_string n) lst
           then fresh_name (n+1)
        else int_to_string n
    in fresh_name 1
*)

let (next_index, reset_index) = 
    let count = ref 0 in
    let n() = (let x = !count in count := x + 1; x) in
    let r() = count := 0 in
    (n,r)

(* CPS: End Fresh name stuff *)
let rec cps_exp e k ke = 
   match e with 
(*[[x]]k,ke = k x*)
     VarExp x -> (VarCPS (k, x))
(*[[c]]k,ke = k x*)
   | ConstExp n -> (ConstCPS (k, n))
(*[[~ e]]k,ke = [[e]](fun r -> k (~ r)), ke, *)
   | MonOpAppExp (m, e) ->
      let r = freshIntName() in
      cps_exp e ((*ClosedCPSCont*)(ContCPS (r, MonOpAppCPS (k, m, r, ke)))) ke
(*[[(e1 + e2)]]k,ke = [[e1]](fun r -> [[e2]](fun s -> k (r + s)),ke),ke*)
   | BinOpAppExp (b, e1, e2) ->
      let r = freshIntName()  in 
      let s = freshIntName()  in 
      let e2CPS =
       cps_exp e2 ((*ClosedCPSCont*)(ContCPS (s, BinOpAppCPS(k, b, r, s, ke)))) ke in
      cps_exp e1 ((*ClosedCPSCont*)(ContCPS (r, e2CPS))) ke
(*[[if e1 then e2 else e3]]k,ke = [[e1]]((fun r -> if r then [[e2]]k,ke else [[e3]]k,ke),ke*)
   | IfExp (e1,e2,e3) ->
      let r =  freshIntName() in 
      let e2cps = cps_exp e2 k ke  in
      let e3cps = cps_exp e3 k ke in 
      cps_exp e1 ((*ClosedCPSCont*)(ContCPS(r, IfCPS(r, e2cps, e3cps)))) ke
(*[[e1 e2]]k,ke = [[e1]](fun r -> [[e2]](fun s -> (r s k ke)),ke),ke*)
   | AppExp (e1,e2) -> 
      let r = freshIntName() in
      let s = freshIntName() in
      let e2cps =
          cps_exp e2 ((*ClosedCPSCont*)(ContCPS (s, AppCPS(k, r, s, ke)))) ke in
      cps_exp e1 ((*ClosedCPSCont*)(ContCPS (r, e2cps))) ke
(*[[fn x => e]]k,ke = k(fnk x kx kes-> [[e]]kx,kes) *)
   | FnExp (x,e) ->
     let (i,j) = (next_index(), next_index()) in
     let ecps = cps_exp e (ContVarCPS i) (ExnContVarCPS j) in
     FnCPS (k, x, i, j, ecps)
(*[[let (dec, e)]]k,ke = <<dec>>([[e]]k,ke),ke *)
   | LetExp (dec,e) -> 
     let ecps = cps_exp e k ke in cps_dec dec ecps ke
(* [[e handle n0 -> e0 | ... | nm -> em]]k,ke =
   [[e]]k, [(n0 |-> [[e0]]k,ke); ... (nm |-> [[em]]k,ke)] + ke *)
   | HandleExp(e, n0, e0, rem_match) ->
     let match_cps =
         List.fold_right
          (fun (n,en) -> fun exn_match ->
           let ecps = cps_exp en k ke in
               (n, ecps)::exn_match)
          ((n0, e0):: rem_match)
          []
     in cps_exp e k (UpdateExnContCSP(match_cps, ke))

(* [[raise e]]k,ke = [[e]](fun r -> match_exn r with ke), ke *)
   | RaiseExp e -> cps_exp e ((*ClosedCPSCont*)(ExnMatch ke)) ke

and
   cps_dec dec ecps ke =
(*   <<val x = e>>ecps,ke = [[e]](fun x -> ecps),ke *)
   match dec with Val (x,e) -> cps_exp e ((*ClosedCPSCont*)(ContCPS (x, ecps))) ke
(*   <<dec1 dec2>>ecps, ke = <<dec1>>(<<dec2>>_ecps,ke),ke *)
   | Seq (dec1,dec2) -> let ecps2 =
     cps_dec dec2 ecps ke in cps_dec dec1 ecps2 ke
   | Local (dec1, dec2) ->  raise (Failure "Not implemented yet")
(*   <<val rec f = fn x => e>>_ecps = \mu x. [[e]]_fun f -> ecps *)
   | Rec (f,x,e) ->
     let (i,j) = (next_index(),next_index()) in
     let ecps2 = cps_exp e (ContVarCPS i) (ExnContVarCPS j) in
     FixCPS ((*ClosedCPSCont*)(ContCPS (f, ecps)), f, x, i, j, ecps2)


(*type system*)

type typeVar = int

let rec expand n (list,len) =
    let q = n / 26 in
        if q = 0 then (n :: list, len + 1)
        else expand q (((n mod 26)::list), len + 1);;

let string_of_typeVar n = 
   let (num_list,len) =
       match (expand n ([],0))
       with ([],l) -> ([],l) (* can't actually happen *)
          | ([s],l) -> ([s],l)
          | (x::xs,l) -> ((x - 1) :: xs, l)
   in
   let s = (String.create len)
   in
   let _ =
    List.fold_left
    (fun n c -> (String.set s n c; n + 1))
    0
    (List.map (fun x -> Char.chr(x + 97)) num_list)  (* Char.code 'a' = 97 *)
   in "'"^s;;

type monoTy = TyVar of typeVar | TyConst of (string * monoTy list)

let rec string_of_monoTy t =
  let rec string_of_tylist = function
     []     -> ""
   | t'::[] -> string_of_monoTy t'
   | t'::ts -> string_of_monoTy t'^ ","^ string_of_tylist ts
  in
  let string_of_subty s =
  match s with 
     TyConst ("*", _) | TyConst ("->", _) -> ("("^ string_of_monoTy s^ ")")
   | _ ->  string_of_monoTy s
  in 
    match t with
       TyVar n         -> (string_of_typeVar n)
     |TyConst (name, []) -> name
     |TyConst (name, [ty]) -> (string_of_subty ty^ " "^ name)
     |TyConst ("*", [ty1; ty2]) -> (string_of_subty ty1^ " * "^ string_of_monoTy ty2)
     |TyConst ("->", [ty1; ty2]) -> (string_of_subty ty1^ " -> "^ string_of_monoTy ty2)
     |TyConst (name, tys) -> ("("^ string_of_tylist tys^ ") "^ name)


let rec accummulate_freeVarsMonoTy fvs ty =
    match ty
    with TyVar n -> n::fvs
       | TyConst (c, tyl) -> List.fold_left accummulate_freeVarsMonoTy fvs tyl

let freeVarsMonoTy ty = accummulate_freeVarsMonoTy [] ty

(*fresh type variable*)
let (fresh, reset) =
   let nxt = ref 0 in
   let f () = (nxt := !nxt + 1; TyVar(!nxt)) in
   let r () = nxt := 0 in
    (f, r)

let bool_ty = TyConst("bool",[])
let int_ty = TyConst ("int", [])
let real_ty = TyConst ("real",[])
let string_ty = TyConst ("string",[])
let unit_ty = TyConst("unit", [])
let mk_pair_ty ty1 ty2 = TyConst("*",[ty1;ty2])
let mk_fun_ty ty1 ty2 = TyConst("->",[ty1;ty2])
let mk_list_ty ty = TyConst("list",[ty])

type polyTy = typeVar list * monoTy  (* the list is for quantified variables *)

let string_of_polyTy (bndVars, t) = match bndVars with [] -> string_of_monoTy t
    | _ ->  (List.fold_left
             (fun s v -> s ^ " " ^ string_of_typeVar v)
             "Forall"
             bndVars)
             ^ ". " ^ string_of_monoTy t

let freeVarsPolyTy ((tvs, ty):polyTy) = set_minus (freeVarsMonoTy ty) tvs

let polyTy_of_monoTy mty = (([],mty):polyTy)

let int_op_ty = polyTy_of_monoTy(mk_fun_ty int_ty (mk_fun_ty int_ty int_ty))
let real_op_ty =
    polyTy_of_monoTy(mk_fun_ty real_ty (mk_fun_ty real_ty real_ty))
let string_op_ty =
    polyTy_of_monoTy(mk_fun_ty string_ty (mk_fun_ty string_ty string_ty))

(* fixed signatures *)
let const_signature const = match const with
   BoolConst b -> polyTy_of_monoTy bool_ty
 | IntConst n -> ([], int_ty)
 | RealConst f -> ([], real_ty)
 | StringConst s -> ([], string_ty)
 | NilConst -> ([0],mk_list_ty (TyVar 0))
 | UnitConst -> ([], unit_ty)

let binop_signature binop = match binop with
     IntPlusOp   -> int_op_ty
   | IntMinusOp   -> int_op_ty
   | IntTimesOp   -> int_op_ty
   | IntDivOp   -> int_op_ty
   | RealPlusOp   -> real_op_ty
   | RealMinusOp   -> real_op_ty
   | RealTimesOp   -> real_op_ty
   | RealDivOp   -> real_op_ty
   | ConcatOp -> string_op_ty
   | ConsOp -> 
       let alpha = TyVar 0
       in ([0], 
              mk_fun_ty alpha (mk_fun_ty (mk_list_ty alpha) (mk_list_ty alpha)))
   | CommaOp ->
       let alpha = TyVar 0 in
       let beta = TyVar 1 in
           ([0;1],
            mk_fun_ty alpha (mk_fun_ty beta (mk_pair_ty alpha beta)))
   | EqOp -> 
     let alpha = TyVar 0 in ([0],mk_fun_ty alpha (mk_fun_ty alpha bool_ty))
   | GreaterOp ->
     let alpha = TyVar 0 in ([0],mk_fun_ty alpha (mk_fun_ty alpha bool_ty))

let monop_signature monop = match monop with
    | HdOp -> let alpha = TyVar 0 in([0], mk_fun_ty (mk_list_ty alpha) alpha)
    | TlOp -> let alpha = TyVar 0 in
                  ([0], mk_fun_ty (mk_list_ty alpha) (mk_list_ty alpha))
    | PrintStringOp -> (([], mk_fun_ty string_ty unit_ty):polyTy)
    | IntNegOp -> ([], mk_fun_ty int_ty int_ty)
    | FstOp -> 
       let alpha = TyVar 0 in
       let beta = TyVar 1 in
           ([0;1], mk_fun_ty (mk_pair_ty alpha beta) alpha)
    | SndOp -> 
       let alpha = TyVar 0 in
       let beta = TyVar 1 in
           ([0;1], mk_fun_ty (mk_pair_ty alpha beta) beta)

(* environments *)
type 'a env = (string * 'a) list

let freeVarsEnv l =
    List.fold_right (fun (_,pty) fvs -> freeVarsPolyTy pty @ fvs) l []

let string_of_env string_of_entry gamma = 
  let rec string_of_env_aux gamma =
    match gamma with
       []        -> ""
     | (x,y)::xs -> x^ " : "^ string_of_entry y^
                    match xs with [] -> "" | _  -> ", "^
                                                   string_of_env_aux xs
  in
    "{"^ string_of_env_aux gamma^ "}"

let string_of_type_env gamma = string_of_env string_of_polyTy gamma

let string_of_val_env string_of_entry gamma = 
  let rec string_of_val_env_aux gamma =
    match gamma with
       []        -> ""
     | (x,y)::xs -> x^ " : "^ string_of_entry y^
                    match xs with [] -> "" | _  -> ", "^
                                                   string_of_val_env_aux xs
  in
    "val "^ string_of_val_env_aux gamma

let string_of_inc_env gamma = string_of_val_env string_of_polyTy gamma

(*environment operations*)
let rec lookup mapping x =
  match mapping with
     []        -> None
   | (y,z)::ys -> if x = y then Some z else lookup ys x


type type_env = polyTy env
let empty_env = ([]:'a env)

let make_env x y = ([(x,y)]: 'a env)
let lookup_env (gamma:'a env) x = lookup gamma x
let sum_env (delta: 'a env) (gamma: 'a env) = ((delta@gamma): 'a env)
let ins_env (gamma: 'a env) x y = sum_env (make_env x y) gamma

(*judgment*)
type judgment =
   ExpJudgment of type_env * exp * monoTy
 | DecJudgment of type_env * dec * type_env

let string_of_judgment judgment =
  match judgment with ExpJudgment(gamma, exp, monoTy) ->
        string_of_type_env gamma ^ " |= "^ string_of_exp exp ^
         " : " ^ string_of_monoTy monoTy
  | DecJudgment (gamma, dec, delta) ->
        string_of_type_env gamma ^ " |= "^ string_of_dec dec ^
         " : " ^ string_of_type_env delta

type proof = Proof of proof list * judgment

(*proof printing*)
let string_of_proof p =
  let depth_max = 10 in
  let rec string_of_struts = function
     []    -> ""
   | x::[] -> (if x then "|-" else "|-")  (* ??? *)
   | x::xs -> (if x then "  " else "| ")^ string_of_struts xs
  in let rec string_of_proof_aux (Proof(ant,conc)) depth lst =
    "\n"^ "  "^ string_of_struts lst^
    (if (depth > 0) then "-" else "")^
    let assum = ant in
      string_of_judgment conc ^
      if depth <= depth_max
         then string_of_assum depth lst assum
      else ""
  and string_of_assum depth lst assum =
    match assum with 
       []     -> ""
     | p'::ps -> string_of_proof_aux p' (depth + 1) (lst@[ps=[]])^
                 string_of_assum depth lst ps
  in
    string_of_proof_aux p 0 []^ "\n\n"

	
type substitution = (typeVar * monoTy) list

let subst_fun (s:substitution) n = (try List.assoc n s with _ -> TyVar n)

(*unification algorithm*)
(* Problem 1 *)
let rec contains n ty =
  match ty with
    TyVar m -> n=m
  | TyConst(st, typelst) ->
     List.fold_left (fun xl x -> if xl then xl else contains n x) false typelst;;

(* Problem 2 *)
let rec substitute ie ty = 
  let n,sub = ie 
  in match ty with
       TyVar m -> if n=m then sub else ty
     | TyConst(st, typelist) -> TyConst(st, List.map (fun t -> substitute ie t) typelist);;

let polyTySubstitute s (pty:polyTy) =
    match s with  (n,residue) ->
    (match pty with (bound_vars, ty) -> 
           if List.mem n bound_vars then pty
           else ((bound_vars, substitute s ty):polyTy))
    

(* Problem 3 *)
let rec monoTy_lift_subst (s:substitution) ty =
  match ty with
    TyVar m -> subst_fun s m
  | TyConst(st, typelst) ->  TyConst(st, List.map (fun t -> monoTy_lift_subst s t) typelst);;

let rec monoTy_rename_tyvars s mty =
    match mty with
      TyVar n -> (match lookup s n with Some m -> TyVar m | _ -> mty)
    | TyConst(c, tys) -> TyConst(c, List.map (monoTy_rename_tyvars s) tys)

let subst_compose (s2:substitution) (s1:substitution) : substitution =
    (List.filter (fun (tv,_) -> not(List.mem_assoc tv s1)) s2) @ 
    (List.map (fun (tv,residue) -> (tv, monoTy_lift_subst s2 residue)) s1)

let gen (env: type_env) ty =
    let env_fvs = freeVarsEnv env in ((set_minus (freeVarsMonoTy ty) env_fvs, ty):polyTy)

let freshInstance ((tvs, ty):polyTy) =
    let fresh_subst = List.fold_right (fun tv s -> ((tv,fresh())::s)) tvs [] in
    monoTy_lift_subst fresh_subst ty

let first_not_in n l =
    let rec first m n l =
        if n > 0 then
         if List.mem m l then first (m+1) n l else m :: (first (m+1) (n - 1) l)
        else []
    in first 0 n l

let alpha_conv ftvs (pty:polyTy) =
    match pty with (btvs, ty) ->
    (let fresh_bvars =
         first_not_in (List.length btvs) (ftvs @ (freeVarsPolyTy pty))
     in (fresh_bvars,
         monoTy_lift_subst (List.combine btvs (List.map (fun v -> TyVar v) fresh_bvars))
         ty))


let polyTy_lift_subst s pty =
	let rec fvsfun x r = match x with
		| TyVar n -> n :: r
		| TyConst (_, l) -> List.fold_right fvsfun l r
	in
	let fvs = List.fold_right fvsfun (snd(List.split s)) [] in
    let (nbvs, nty) = alpha_conv fvs pty in
    ((nbvs, monoTy_lift_subst s nty):polyTy)

let rec mk_bty_renaming n bty =
    match bty with [] -> ([],[])
    | (x::xs) -> (match mk_bty_renaming (n-1) xs
                   with (s,l) -> (((x,n) :: s), n :: l))

let polyTy_rename_tyvars s (bty, mty) =
    let (renaming,new_bty) = mk_bty_renaming (~-7) bty in
    (new_bty, monoTy_rename_tyvars s (monoTy_rename_tyvars renaming mty))


let env_lift_subst s (env: 'a env) =
    ((List.map (fun (x,polyTy) -> (x,polyTy_lift_subst s polyTy)) env): 'a env)

let env_rename_tyvars s (env: 'a env) =
    ((List.map
      (fun (x,polyTy) -> (x,polyTy_rename_tyvars s polyTy)) env): 'a env)

(* Problem 4 *)
let rec unify eqlst : substitution option =
  let rec addNewEqs lst1 lst2 acc =
    match lst1,lst2 with
      [],[] -> Some acc
    | t::tl, t'::tl' -> addNewEqs tl tl' ((t,t')::acc)
    | _ -> None
  in
  match eqlst with
    [] -> Some([])
    (* Delete *)
  | (s,t)::eqs when s=t -> unify eqs
    (* Eliminate *)
  | (TyVar(n),t)::eqs when not(contains n t)-> 
      let eqs' = List.map (fun (t1,t2) -> (substitute (n,t) t1 , substitute (n,t) t2)) eqs
      in (match unify eqs' with
           None -> None
         | Some(phi) -> Some((n, monoTy_lift_subst phi t):: phi))
    (* Orient *)
  | (TyConst(str, tl), TyVar(m))::eqs -> unify ((TyVar(m), TyConst(str, tl))::eqs)
    (* Decompose *)
  | (TyConst(str, tl), TyConst(str', tl'))::eqs when str=str' -> 
      (match (addNewEqs tl tl' eqs) with
        None -> None
      | Some l -> unify l)
    (* Other *)
  | _ -> None
;;


(*-----------------------------------------------*)

(*constraint list*)
type consList = (monoTy * monoTy) list

(*applying a substitution to a proof*)
let rec proof_lift_subst f = function
    Proof(assum, ExpJudgment(gamma, exp, monoTy)) ->
    Proof(List.map (proof_lift_subst f) assum,
          ExpJudgment(env_lift_subst f gamma, exp, monoTy_lift_subst f monoTy))
 | Proof(assum, DecJudgment(gamma, dec, delta)) ->
    Proof(List.map (proof_lift_subst f) assum,
          DecJudgment(env_lift_subst f gamma, dec, env_lift_subst f delta))

let rec proof_rename_tyvars f = function
    Proof(assum, ExpJudgment(gamma, exp, monoTy)) ->
    Proof(List.map (proof_rename_tyvars f) assum,
          ExpJudgment(env_rename_tyvars f gamma, exp,
                      monoTy_rename_tyvars f monoTy))
 | Proof(assum, DecJudgment(gamma, dec, delta)) ->
    Proof(List.map (proof_rename_tyvars f) assum,
          DecJudgment(env_rename_tyvars f gamma, dec,
                      env_rename_tyvars f delta))


let get_ty = function
   None       -> raise(Failure "None")
 | Some(ty,p) -> ty

let get_proof = function
   None       -> raise(Failure "None")
 | Some(ty,p) -> p

let infer_exp gather_exp (gamma:type_env) (exp:exp) = 
  let ty = fresh() in
  let result = 
    match gather_exp gamma exp ty with
       None         -> None
     | Some(proof,sigma) -> match ty with
          | TyVar n -> Some (subst_fun sigma n, proof_lift_subst sigma proof)
          | _       -> None
  in let _ = reset() in
  result;;

let infer_dec gather_dec (gamma:type_env) (dec:dec) =
  let result = 
    match gather_dec gamma dec with
       None -> None
     | Some(proof,delta,sigma) -> Some (env_lift_subst sigma delta, proof_lift_subst sigma proof) 
  in result;; (*let _ = reset() in*)
  (*(result: (type_env * proof) option)*)

let string_of_constraints c =
  let rec aux c =
     match c with 
     | [] -> ""
     | [(s,t)] ->  (string_of_monoTy s^ " --> "^ string_of_monoTy t)
     | (s,t)::c' -> (string_of_monoTy s^ " --> "^ string_of_monoTy t^
		     "; "^ aux c')
  in ("["^ aux c^ "]\n")

let string_of_substitution s =
  let rec aux s =
     match s with 
     | [] -> ""
     | [(i,t)] -> ((string_of_typeVar i)  ^ " --> " ^ string_of_monoTy t)
     | (i,t)::s' -> (((string_of_typeVar i)  ^ " --> ")^
                     string_of_monoTy t^ "; "^ aux s')
  in ("["^ aux s^ "]\n")

let niceInfer_exp gather_exp (gamma:type_env) exp = 
  let ty = fresh()
  in
  let result = 
    match gather_exp gamma exp ty with
     None ->
      (print_string("Failure: No type for expression: "^
       string_of_exp exp^ "\n"^
       "in the environment: "^
       string_of_env string_of_polyTy gamma^ "\n");
       raise (Failure ""))
   | Some (p,s) ->
   (string_of_proof p^
	(*
   "Constraints: "^
   string_of_constraints c ^
   "Unifying..."^
   match unify c with
     None -> ("Failure: No solution for these constraints!\n"^
              raise (Failure ""))
   | Some s ->
	*)
   ("Unifying substitution: "^
    string_of_substitution s^
    "Substituting...\n"^
    let new_p = proof_lift_subst s p in
    string_of_proof new_p)) in
  let _ = reset() in
  result;;

let niceInfer_dec
    (gather_dec:(type_env -> dec -> (proof * type_env * substitution) option))
    (gamma:type_env) dec = 
  let result = 
    match gather_dec gamma dec with
     None ->
      (print_string("Failure: No type for declaraion: "^
       string_of_dec dec^ "\n"^
       "in the environment: "^
       string_of_env string_of_polyTy gamma^ "\n");
       raise (Failure ""))
   | Some (p,d,s) ->
   (string_of_proof p^
   ("Unifying substitution: "^
    string_of_substitution s^
    "Substituting...\n"^
    let new_p = proof_lift_subst s p in
    string_of_proof new_p)) in
  let _ = reset() in
  result;;

(* Collect all the TyVar indices in a proof *)

let rec collectTypeVars ty lst =
  match ty with
    TyVar m -> m::lst
  | TyConst(st, typelst) -> List.fold_left (fun xl x -> collectTypeVars x xl) lst typelst


let rec collectFreeTypeVars bty ty lst =
  match ty with
    TyVar m -> if List.mem m bty then lst else m::lst
  | TyConst(st, typelst) ->
    List.fold_left (fun xl x -> collectFreeTypeVars bty x xl) lst typelst

let collectPolyTyVars (bty,mty) lst = collectFreeTypeVars bty mty lst

let collectEnvVars (gamma:type_env) lst =
    List.fold_left (fun tys (_,pty)-> collectPolyTyVars pty tys) lst gamma

let collectJdgVars jdg lst =
    match jdg with ExpJudgment(gamma, exp, monoTy) ->
        collectEnvVars gamma (collectTypeVars monoTy lst)
    | DecJudgment(gamma, dec, delta) ->
        collectEnvVars gamma (collectEnvVars delta lst)

let rec collectProofVars prf lst =
  match prf with Proof (assum, jdg)
   -> collectAssumVars assum (collectJdgVars jdg lst)
and collectAssumVars assum lst =
  match assum with 
    []     -> lst
  | p::ps -> collectAssumVars ps (collectProofVars p lst)

(* Rename all the variables in a proof in a canonical way *)
let rec drop y = function
   []    -> []
 | x::xs -> if x=y then drop y xs else x::drop y xs

let rec delete_duplicates = function
   []    -> []
 | x::xs -> x::delete_duplicates (drop x xs)

let canonicalize_proof prf_opt =
    match prf_opt with None -> None
    | Some(ty, prf) ->
  let (varlst,_) =
    List.fold_right (fun x (xl,idx) -> ((x,idx)::xl), idx+1) 
      (delete_duplicates (collectProofVars prf (collectTypeVars ty []))) 
      ([],1)
  in Some(monoTy_rename_tyvars varlst ty, proof_rename_tyvars varlst prf)

let canon = canonicalize_proof

let canon_dec prf_opt =
    match prf_opt with None -> None
    | Some prf ->
  let (varlst,_) =
    List.fold_right (fun x (xl,idx) -> ((x, idx)::xl), idx+1) 
      (delete_duplicates (collectProofVars prf []))
      ([],1)
  in Some(proof_rename_tyvars varlst prf)

  (*--------------------------------mp6 start-----------------------------*)
let rec gather_exp_ty_substitution gamma exp tau =
    let judgment = ExpJudgment(gamma, exp, tau) in
(*
    let _ = print_string ("Trying to type "^ string_of_judgment judgment^"\n") in
*)
    let result =
    match exp
    with ConstExp c ->
         let tau' = const_signature c in
         (match unify [(tau, freshInstance tau')]
          with None       -> None
             | Some sigma -> Some(Proof([],judgment), sigma))
    | VarExp x -> 
      (match lookup_env gamma x with None -> None
       | Some tau' ->
         (match unify [(tau, freshInstance tau')]
          with None       -> None
             | Some sigma -> Some(Proof([],judgment), sigma)))
    | MonOpAppExp (monop, e1) ->
      let tau' = monop_signature monop in
      let tau1 = fresh() in
      (match gather_exp_ty_substitution gamma e1 tau1
       with None -> None
       | Some(pf, sigma) ->
         (match unify[(monoTy_lift_subst sigma (mk_fun_ty tau1 tau),
                       freshInstance tau')]
          with None -> None
          | Some subst ->
            Some(Proof([pf], judgment),
                 subst_compose subst sigma)))
    | BinOpAppExp (binop, e1,e2) ->
      let tau' = binop_signature binop in
      let tau1 = fresh() in
      (match gather_exp_ty_substitution gamma e1 tau1
       with None -> None
       | Some(pf1, sigma1) ->
         let tau2 = fresh() in
         (match gather_exp_ty_substitution (env_lift_subst sigma1 gamma) e2 tau2
          with None -> None
          | Some (pf2, sigma2) ->
            let sigma21 = subst_compose sigma2 sigma1 in
            (match unify[(monoTy_lift_subst sigma21
                          (mk_fun_ty tau1 (mk_fun_ty tau2 tau)),freshInstance tau')]
             with None -> None
             | Some sigma3 -> 
               Some(Proof([pf1;pf2], judgment),subst_compose sigma3 sigma21))))
    | IfExp(e1,e2,e3) ->
      (match gather_exp_ty_substitution gamma e1 bool_ty
       with None -> None
       | Some(pf1, sigma1) ->
         (match gather_exp_ty_substitution
                (env_lift_subst sigma1 gamma) e2 (monoTy_lift_subst sigma1 tau)
          with None -> None
          | Some (pf2, sigma2) ->
            let sigma21 = subst_compose sigma2 sigma1 in
            (match gather_exp_ty_substitution
                   (env_lift_subst sigma21 gamma) e3
                   (monoTy_lift_subst sigma21 tau)
             with  None -> None
             | Some(pf3, sigma3) ->
               Some(Proof([pf1;pf2;pf3], judgment), subst_compose sigma3 sigma21))))
    | FnExp(x,e) ->
      let tau1 = fresh() in
      let tau2 = fresh() in
      (match gather_exp_ty_substitution
             (ins_env gamma x (polyTy_of_monoTy tau1)) e tau2
       with None -> None
       | Some (pf, sigma) ->
         (match unify [(monoTy_lift_subst sigma tau,
                        monoTy_lift_subst sigma (mk_fun_ty tau1 tau2))]
          with None -> None
          | Some sigma1 ->
            Some(Proof([pf],judgment), subst_compose sigma1 sigma)))
    | AppExp(e1,e2) ->
      let tau1 = fresh() in
      (match gather_exp_ty_substitution gamma e1 (mk_fun_ty tau1 tau)
       with None -> None
       | Some(pf1, sigma1) ->
         (match gather_exp_ty_substitution (env_lift_subst sigma1 gamma) e2
                                           (monoTy_lift_subst sigma1 tau1)
          with None -> None
          | Some (pf2, sigma2) ->
            Some(Proof([pf1;pf2], judgment), subst_compose sigma2 sigma1)))
    | LetExp(dec,e) ->
      (match gather_dec_ty_substitution gamma dec
       with None -> None
       | Some (pf1,delta,sigma1) ->
         (match gather_exp_ty_substitution 
                (sum_env delta (env_lift_subst sigma1 gamma))
                e
                (monoTy_lift_subst sigma1 tau)
          with None -> None
          | Some (pf2, sigma2) ->
            Some (Proof([pf1;pf2], judgment), subst_compose sigma2 sigma1)))
    | RaiseExp e ->
      (match gather_exp_ty_substitution gamma e int_ty
       with None -> None
       | Some(pf, sigma) -> Some(Proof([pf],judgment), sigma))
    | HandleExp (e,intopt1,e1, match_list) ->
      (match (gather_exp_ty_substitution gamma e tau)
       with None -> None
       | Some (pf, sigma) ->
         (match
           List.fold_left
           (fun part_result -> fun (intopti, ei) ->
            (match part_result with None -> None
             | Some (rev_pflist, comp_sigmas) ->
               (match gather_exp_ty_substitution
                      (env_lift_subst comp_sigmas gamma) ei
                      (monoTy_lift_subst comp_sigmas tau)
                with None -> None
                | Some (pfi, sigmai) ->
                  Some (pfi :: rev_pflist, subst_compose sigmai comp_sigmas))))
           (Some([pf], sigma))
           ((intopt1,e1):: match_list)
           with None -> None
           | Some (rev_pflist, comp_subst) ->
             Some(Proof(List.rev rev_pflist, judgment), comp_subst)))
in  result

and gather_dec_ty_substitution gamma dec =

    (*let _ = print_string ("Trying to type declaration "^ string_of_dec dec ^ "
 in environment "^string_of_type_env gamma^"\n") in*)
    let result = 
    match dec
    with Val(x,e) ->
      let tau = fresh() in
      (match gather_exp_ty_substitution gamma e tau
       with None -> None
       | Some(pf, sigma) ->
		let delta = if x = "" then env_lift_subst sigma gamma
		(*| let delta =*) else make_env x (gen (env_lift_subst sigma gamma) 
                                     (monoTy_lift_subst sigma  tau)) in
         Some(Proof([pf], DecJudgment (gamma, dec, delta)), delta, sigma))
    | Rec(f,x,e) ->
      let tau1 = fresh() in
      let tau2 = fresh() in
      let t1_arrow_t2 = mk_fun_ty tau1 tau2 in
      (match gather_exp_ty_substitution 
             (ins_env (ins_env gamma x (polyTy_of_monoTy tau1)) 
                      f (polyTy_of_monoTy t1_arrow_t2))
             e tau2
       with None -> None
       | Some(pf, sigma) ->
         let delta = make_env f (gen (env_lift_subst sigma gamma) 
                                     (monoTy_lift_subst sigma t1_arrow_t2))
         in Some (Proof([pf], DecJudgment (gamma, dec, delta)), delta, sigma))
    | Seq(dec1,dec2) ->
      (match gather_dec_ty_substitution gamma dec1
       with None -> None
       | Some (pf1,delta1,sigma1) ->
         (match gather_dec_ty_substitution
                (env_lift_subst sigma1 (sum_env delta1 gamma))
                dec2
          with None -> None
          | Some (pf2,delta2,sigma2) ->
            let sigma21 = subst_compose sigma2 sigma1 in
            let delta21 = env_lift_subst sigma21 (sum_env delta2 delta1) in
            Some (Proof([pf1; pf2], DecJudgment (gamma, dec, delta21)), delta21,
                  sigma21)))
    | Local(dec1, dec2) ->
      (match gather_dec_ty_substitution gamma dec1
       with None -> None
       | Some (pf1,delta1,sigma1) ->
         (match gather_dec_ty_substitution
                (env_lift_subst sigma1 (sum_env delta1 gamma))
                dec2
          with None -> None
          | Some (pf2,delta2,sigma2) ->
            let sigma21 = subst_compose sigma2 sigma1 in
            let delta2 = env_lift_subst sigma21 delta2 in
            Some (Proof([pf1; pf2], DecJudgment (gamma, dec, delta2)), delta2,
                  subst_compose sigma2 sigma1)))
in
    result

(*--------------------------------mp6 end-----------------------------*)
	
type value =
    UnitVal                             
  | BoolVal of bool
  | IntVal of int                       
  | RealVal of float
  | StringVal of string                 
  | PairVal of value * value
  | ListVal of value list 
  | CPSClosureVal of string * int * int * exp_cps * cps_env_entry list
  | CPSRecClosureVal of string * string * int * int * exp_cps * cps_env_entry list

and cps_env_entry =
     ValueBinding of (string * value)
   | ContBinding of (int * (cps_cont * cps_env_entry list))
   | ExnContBinding of (int * (exn_cont * cps_env_entry list))

let rec lookup_value env x =
   match env with [] -> None
   | (bnd:: rem_env) ->
    (match bnd with ValueBinding(y,v) ->
      if x = y then Some v else lookup_value rem_env x
     | _ -> lookup_value rem_env x)

let rec lookup_cont env x =
   match env with [] -> None
   | (bnd:: rem_env) ->
    (match bnd with ContBinding(y,c) ->
      if x = y then Some c else lookup_cont rem_env x
     | _ -> lookup_cont rem_env x)

let rec lookup_exn_cont env x =
   match env with [] -> None
   | (bnd:: rem_env) ->
    (match bnd with ExnContBinding(y,ec) ->
      if x = y then Some ec else lookup_exn_cont rem_env x
     | _ -> lookup_exn_cont rem_env x)

let rec string_of_value v =
   match v with
    UnitVal           -> "()"
  | IntVal n          -> string_of_int n 
  | RealVal r         -> string_of_float r
  | BoolVal true      -> "true"
  | BoolVal false     -> "false"
  | StringVal s       ->  ("\"" ^ s ^ "\"")
  | PairVal (v1,v2)   -> "("^
                         string_of_value v1^  ", "^
                         string_of_value v2^
                          ")"
  | ListVal l         -> "["^
                         (let rec pl = function
                              []     -> "]"
                              | [v1] -> string_of_value v1 ^ "]"
                              | v1 :: vl -> string_of_value v1 ^ "; " ^ pl vl
                              in pl l)
  | CPSClosureVal (x, i, j, e, m) ->  ("<some closure>")
  | CPSRecClosureVal (f, x, i, j, e, m)  ->  ("<some recvar>")

let print_value v = print_string (string_of_value v)


(* Basic Evaulation Support *)

let const_to_val c = 
  match c with
    BoolConst b   -> BoolVal b
  | IntConst i    -> IntVal i
  | RealConst f   -> RealVal f
  | StringConst s -> StringVal s
  | NilConst      -> ListVal []
  | UnitConst     -> UnitVal

type ord = GT (* greater *) | EQ (* equal *) | LT (* less *)

let gencompare a b =
  if a < b then LT
  else if a > b then GT
  else EQ

let rec compareVal v1 v2 =
   match (v1, v2) with
    (UnitVal, UnitVal) -> EQ
  | (BoolVal b1, BoolVal b2) -> gencompare b1 b2
  | (IntVal n1, IntVal n2) -> gencompare n1 n2
  | (RealVal r1, RealVal r2) -> gencompare r1 r2
  | (StringVal s1, StringVal s2) -> gencompare s1 s2
  | (PairVal (u1,u2), PairVal (v1,v2)) -> paircompare (u1,u2) (v1,v2)
  | (ListVal l1, ListVal l2) -> listcompare l1 l2
  | (CPSClosureVal _, _) -> raise (Failure "Cannot compare funtions")
  | (_, CPSClosureVal _) -> raise (Failure "Cannot compare funtions")
  | (CPSRecClosureVal _, _) -> raise (Failure "Cannot compare funtions")
  | (_, CPSRecClosureVal _) -> raise (Failure "Cannot compare funtions")
  | (_, _) -> raise (Failure "Type checking error")

and paircompare (u1, u2) (v1, v2) =
     (match compareVal u1 v1 with EQ -> compareVal u2 v2
      | r -> r)

and listcompare l1 l2 =
   match l1 with
     [] -> (match l2 with [] -> EQ | _ -> LT)
   | (x::xs) -> (match l2 with (y::ys) ->
                   (match compareVal x y with EQ -> listcompare xs ys
                     | r -> r)
                  | [] -> GT)

type val_or_exn = Value of value | Exn of int

let binOpApply binop v1 v2 =
  match (binop, v1, v2) with
    (IntPlusOp, IntVal n1, IntVal n2)      -> Value(IntVal (n1 + n2))
  | (IntMinusOp, IntVal n1, IntVal n2)     -> Value(IntVal (n1 - n2))
  | (IntTimesOp, IntVal n1, IntVal n2)     -> Value(IntVal (n1 * n2))
  | (IntDivOp, IntVal n1, IntVal n2)       -> 
    if n2 = 0 then Exn 0 (* Part 2, Problem 16 *) else Value(IntVal (n1 / n2))
  | (RealPlusOp, RealVal n1, RealVal n2)   -> Value(RealVal (n1 +. n2))
  | (RealMinusOp, RealVal n1, RealVal n2)  -> Value(RealVal (n1 -. n2))
  | (RealTimesOp, RealVal n1, RealVal n2)  -> Value(RealVal (n1 *. n2))
  | (RealDivOp, RealVal n1, RealVal n2)    ->
    if n2 = 0.0 then Exn 0 (* Part 2, Problem 16 *) else Value(RealVal (n1 /. n2))
  | (ConcatOp, StringVal s1, StringVal s2) -> Value(StringVal (s1 ^ s2))
  | (ConsOp, v, ListVal l)                 -> Value(ListVal (v::l))
  | (CommaOp, u1, u2)                      -> Value(PairVal (u1,u2))
  | (EqOp, u1, u2)                         -> Value(BoolVal (match compareVal u1 u2 with EQ -> true
                                                        | _ -> false))
  | (GreaterOp, u1, u2)                    -> Value(BoolVal (match compareVal u1 u2 with GT -> true
                                                        | _ -> false))
  | (_, _, _)                              -> raise (Failure "typechecking error")

let monOpApply op v =
  match (op, v) with
  | (IntNegOp, IntVal n)         -> Value(IntVal (- n))
 (* Part 2, Problem 16 *)
  | (HdOp, ListVal [])         -> Exn 0
  | (HdOp, ListVal (v::vs))    -> Value(v)
 (* Part 2, Problem 16 *)
  | (TlOp, ListVal [])         -> Exn 0
  | (TlOp, ListVal (v::vs))    -> Value(ListVal vs)
  | (FstOp, PairVal (v1, v2))  -> Value(v1)
  | (SndOp, PairVal (v1, v2))  -> Value(v2)
  | (PrintStringOp, StringVal s) -> (print_string s; Value(UnitVal))
  | (_ , _ )                   -> failwith "monOpApply: bad input"

(*-----------------------Type for CPS Transition Semantics results---------------*)

type step_result =
   Intermediate of (cps_env_entry list * exp_cps)
 | Final of value
 | UncaughtException of int
 | Failed
