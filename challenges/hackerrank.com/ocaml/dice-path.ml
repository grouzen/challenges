
(* Can't find a "Optimal substructure for this problem", so I can't *)
(* apply DP here. *)
  
type dice = {
  top    : int;
  bottom : int;
  left   : int;
  right  : int;
  front  : int;
  back   : int;
};;

type 'a bin_tree =
  | Leaf
  | Node of 'a bin_tree * 'a * 'a bin_tree;;
  
type roll_direction = Right | Down;;

let init_dice =
  {
    top    = 1;
    bottom = 6;
    left   = 3;
    right  = 4;
    front  = 2;
    back   = 5;
  };;
  
let roll_dice dice = function
  | Right ->
     {
       top    = dice.left;
       bottom = dice.right;
       left   = dice.bottom;
       right  = dice.top;
       front  = dice.front;
       back   = dice.back
     }
  | Down  ->
     {
       top    = dice.back;
       bottom = dice.front;
       left   = dice.left;
       right  = dice.right;
       front  = dice.top;
       back   = dice.bottom
     };;
    
let build size dice pos =
  let rec build_cps size dice pos k =
    let (m,   n) = size in
    let (pm, pn) = pos  in
    
    if pm > m || pn > n then k Leaf
    else
      build_cps size (roll_dice dice Right) (pm, pn + 1)
                (fun dr ->
                 build_cps size (roll_dice dice Down) (pm + 1, pn)
                           (fun dd -> k (Node (dr, dice.top, dd))))
  in
  build_cps size dice pos (fun x -> x);;

let find_max tree =
  let rec find_max_cps tree k =
    match tree with
    | Node (left, n, right) ->
       find_max_cps left (fun dl -> find_max_cps right (fun dr -> k (n + (max dl dr))))
    | Leaf -> k 0
  in
  find_max_cps tree (fun x -> x);;
  
let rec run_field size =
  let init_pos  = (1, 1) in
  let bb = build size init_dice init_pos in
  Printf.printf "%d\n" (find_max bb);;

let () =
  let t = int_of_string (read_line ()) in
  (* reads a single size tuple *)
  let read_p () =
    Scanf.sscanf (read_line ()) "%d %d" (fun x y -> (x, y)) in
  
  (* reads N size tuples from the stdin and returns (int * int) list *)
  let rec read_ps acc = function
    | 0 -> acc
    | n -> read_ps (read_p () :: acc) (n - 1) in

  List.iter run_field (List.rev (read_ps [] t));;

