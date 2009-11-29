(* The number of iterations *)
(* let iter = 800 *)

let iter = print_string "Number of iterations [500]: "; 
           try read_int ()
           with Failure f -> 500

let _ = 
  let width = 2 * iter - 1 and height = iter in
    Graphics.open_graph (" " ^ (string_of_int width) ^ "x" ^ (string_of_int height))

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
  | Full,  Full,  Full -> Empty

let rec next_gen prev cur tail =
  match tail with
  | [] -> (rule prev cur Empty) :: (rule cur Empty Empty) :: []
  | head :: new_tail -> (rule prev cur head) :: (next_gen cur head new_tail)
(*
  if (tail = []) then
    (rule prev cur Empty) :: (rule cur Empty Empty) :: []
  else
    let head = List.hd tail in
    let new_tail = List.tl tail in
      (rule prev cur head) :: (next_gen cur head new_tail)
 *)

let make_gen start =
  let head = List.hd start in
  let tail = List.tl start in
    (rule Empty Empty head) :: (next_gen Empty head tail)

let rec show_gen line x y =
  let head = List.hd line in
    if (head = Full) then Graphics.plot x y;
    let tail = List.tl line in
      if (tail != []) then show_gen tail (x + 1) y

let rec next_generation count start =
  show_gen start count count;
  if (count > 1) then next_generation (count - 1) (make_gen start) else ()

let _ = next_generation iter [Full]
let _ = Graphics.read_key ()
