type cell = Empty | Full

let rule a b c =
  match (a, b, c) with
  | Empty, Empty, Empty -> Empty 
  | Empty, Empty, Full -> Full  
  | Empty, Full,  Empty -> Full  
  | Empty, Full,  Full -> Empty 
  | Full,  Empty, Empty -> Full  
  | Full,  Empty, Full -> Empty 
  | Full,  Full,  Empty -> Full  
  | Full,  Full,  Full -> Empty ;;

exception ProgrammingError of int

let rec next_gen prev state =
  match state with
  | c :: [] -> (rule prev c Empty) :: (rule c Empty Empty) :: []
  | c :: h :: tail ->
      (rule prev c h) :: (next_gen c (h :: tail))
  | _ -> raise (ProgrammingError 0) (* impossible *)

let make_gen start =
  match start with
  | head :: tail -> (rule Empty Empty head) :: (next_gen Empty start)
  | _ -> raise (ProgrammingError 1) (* impossible *)

let rec show_gen line x y =
  match line with
  | head :: tail ->
      let _ = if (head = Full) then Graphics.plot x y else () in
        show_gen tail (x + 1) y
  | _ -> ()

let rec next_generation count start =
  let _ = show_gen start count count in
    if (count = 1) then () else next_generation (count - 1) (make_gen start)



let _ = 
  let iter = print_string "Number of iterations [500]: " ; 
             try read_int ()
             with Failure f -> 500 in
  let _ = 
    let width = string_of_int (2 * iter - 1) and height = string_of_int iter in
      Graphics.open_graph (" " ^ width ^ "x" ^ height) in
    next_generation iter [Full]

let _ = Graphics.read_key ()
