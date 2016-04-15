(* reverse a list *)

let rev_list list =
  let rec rev acc = function
  | []    -> acc
  | x::xs -> rev (x::acc) xs in
  rev [] list;;

let () =

  let rec read_to_list lst =
    try
      read_to_list (List.append lst [int_of_string (read_line ())])
    with
      End_of_file -> lst
  in

  let lst = rev_list (read_to_list []) in
  List.iter (fun n -> Printf.printf "%d\n" n) lst;;


  
