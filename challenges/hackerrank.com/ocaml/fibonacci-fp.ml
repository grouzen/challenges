(* Ocaml sucks, fockin hashtable can't handle Big_int values, *)
(* and it fails in run-time with fockin error! So, fock this shit! *)

open Big_int;;

let fib_memo : int -> big_int = fun n ->
  let rec fib m : big_int -> big_int = fun n ->
    try Hashtbl.find m n with
    | Not_found -> 
       if Big_int.le_big_int n (Big_int.big_int_of_int 2) then n
       else
         let r = Big_int.add_big_int
                   (fib m (Big_int.sub_big_int n (Big_int.big_int_of_int 1)))
                   (fib m (Big_int.sub_big_int n (Big_int.big_int_of_int 2))) in
         Hashtbl.add m n r;
         r
  in
  fib (Hashtbl.create n) (big_int_of_int n);;
  
                                       
let rec fib = function
  | 0 -> 0
  | 1 -> 1
  | n -> fib (n - 1) + fib (n - 2);;
  
