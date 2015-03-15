(* CS421 - Spring 2014
 * MP1
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You will want to change how a number of these start.

Group with Yifang Zhang - zhang303
 *)

open Mp1common

(* Problem 1 *)
let title = "MP 1 -- Basic OCaml" (* Change this *)

(* Problem 2 *)
let e = 2.71828  (* Change this *)

(* Problem 3 *)
let firstFun n = (n*2)+5

(* Problem 4 *)
let divide_e_by x = e /. x

(* Problem 5 *)
let diff_square_9 m = if m < 3 && m > (-3) then m*m-9
			else 9-m*m

(* Problem 6 *)
let dist_double s n = print_string (s ^ ", I guess it's double or nothing!\n"); 2*n
(* Problem 7 *)
let swizzle (w,x,y,z) = (z,y,w,x)

(* Problem 8 *)
let left_right_compose f g = fun x -> (f (g (f x)))
