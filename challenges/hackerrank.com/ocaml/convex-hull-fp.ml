

(* since OCaml doesn't give us such constant, let's define it *)
let pi = 4. *. atan 1.;;

let perimeter hull =
  let rec sections acc = function
    | []     -> acc
    | hd::[] -> sections ((hd, fst (List.hd (List.rev acc))) :: acc) []
    | hd::tl -> sections ((hd, List.hd tl) :: acc) tl
  in
  let section_len sec =
    let (dx,dy) = (float_of_int (abs (fst (fst sec) - fst (snd sec))),
                   float_of_int (abs (snd (fst sec) - snd (snd sec)))) in
    sqrt ((dx ** 2.) +. (dy ** 2.))
  in

  List.fold_right (+.) (List.map section_len (sections [] hull)) 0.;;

let solution_gift_wrapping points =
  (* get the largest, and the least points *)
  let extremum fn a b =
    match (a,b) with
    | ((x1,y1),(x2,y2)) ->
       let (y,x) = fn (y1,x1) (y2,x2) in
       (x,y)
  in
  let p1 = List.fold_left (extremum min) (List.hd points) points in
  let q1 = List.fold_left (extremum max) (List.hd points) points in

  let rec get_angles side cp acc = function
    | []    -> acc
    | hd::tl ->
       let polar_angle cp np =
         let (nx,ny) =
           let ddp = ((fst np) - (fst cp), (snd np) - (snd cp)) in
           match (side,ddp) with
           | (0,ddp) -> ddp
           | (_,(x,y)) -> (-x,y)
         in
         let (dx,dy) = (float_of_int nx, float_of_int ny) in
         let a = atan (dy /. dx) in
         Printf.printf "(%d,%d) dx: %f, dy: %f\n" (fst np) (snd np) dx dy;
         match (dx,dy) with
         | (x,y) when x >  0. && y < 0. -> a +. (2. *. pi)
         | (x,y) when x < 0. -> a +. pi
         | _ -> a
       in
       let pa = polar_angle cp hd in
       get_angles side cp ((hd,pa)::acc) tl
  in

  let wrap lp hp s =
    let rec aux cp side hull = function
      | [] -> cp :: hull
      | s  ->
         (* let ccp = match (side,cp) with *)
         (*   | (1,(x,y)) -> (-x,y) *)
         (*   | (_,(x,y)) -> (x,y) in *)
         Printf.printf "Current point: (%d,%d)\n" (fst cp) (snd cp);
         
         let angles = List.sort
                        (fun (_,a1) (_,a2)  -> compare a1 a2)
                        (get_angles side cp [] s) in
         let (nx,ny) = fst (List.hd angles) in

         print_string "angles:\n";
         List.iter
           (fun ((x,y),a) -> Printf.printf "(%d,%d) %f\n" x y a) angles;

         match lp with
         | (lx,ly) when nx == lx && ny == ly -> aux cp side hull []
         | _ ->
            let sside = match (side,(nx,ny)) with
              | (0,(x,y)) when x = fst hp && y == snd hp -> 1
              | (side,_) -> side
            in
            Printf.printf "side: %d\n" sside;
            aux (nx,ny) sside (cp :: hull) (cp :: (List.tl (List.map fst angles)))

    in
    aux lp 0 [] s
  in

  let hull = wrap p1 q1
                  (List.filter
                     (fun (x,y) -> x != fst p1 && y != snd p1)
                     points) in
  (* print_string "Hull:\n"; *)
  (* List.iter (fun (x,y) -> Printf.printf "(%d,%d)\n" x y) hull; *)
  perimeter hull;;

let () =

  let n = int_of_string (read_line ()) in

  (* reads a single point *)
  let read_p () =
    Scanf.sscanf (read_line ()) "%d %d" (fun x y -> (x, y)) in

  (* reads N points from the stdin and returns (int * int) list *)
  let rec read_ps acc = function
    | 0 -> acc
    | n -> read_ps (read_p () :: acc) (n - 1) in

  Printf.printf "%.1f\n" (solution_gift_wrapping (read_ps [] n));;
