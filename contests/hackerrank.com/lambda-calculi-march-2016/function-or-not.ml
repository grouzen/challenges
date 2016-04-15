
let validate fn =
  let mem = Hashtbl.create (List.length fn) in
  let rec validate' mem = function
    | []          -> true
    | (x,y) :: tl ->
       let my = try Hashtbl.find mem x with
                | Not_found -> Hashtbl.add mem x y; y
       in
       if my != y then false
       else validate' mem tl  
  in
  validate' mem fn;;
  
let rec go = function
  | []       -> ()
  | hd :: tl ->
     if validate hd then print_endline "YES"
     else print_endline "NO";
     go tl;;
        
let () =
  let read_fn () =
    let n = int_of_string (read_line ()) in
    
    let read_rel () = Scanf.sscanf (read_line ())
                                "%d %d" (fun x y -> (x, y)) in
    let rec read_rels acc = function
      | 0 -> acc
      | n -> read_rels (read_rel () :: acc) (n - 1)
    in

    read_rels [] n
  in
  let rec read_fns acc = function
    | 0 -> acc
    | n -> read_fns (read_fn () :: acc) (n - 1)
  in
  
  let t = int_of_string (read_line ()) in
  go (List.rev (read_fns [] t));;
    
