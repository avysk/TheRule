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

let rec next_gen prev = function
  | c :: [] -> (rule prev c Empty) :: (rule c Empty Empty) :: []
  | c :: h :: tail ->
      (rule prev c h) :: (next_gen c (h :: tail))
  | _ -> failwith "Internal error"

let make_gen = function
  | head :: tail as input -> (rule Empty Empty head) :: (next_gen Empty input)
  | _ -> failwith "Internal error"

let rec show_gen line x y =
  match line with
  | head :: tail ->
      if (head = Full) then Graphics.plot x y else () ;
      show_gen tail (x + 1) y
  | _ -> ()

let rec next_generation count start =
  show_gen start count count ;
  if (count = 1) then ()
  else next_generation (count - 1) (make_gen start)

let main () =
  let iter =
    try
      print_string "Number of iterations [500]: ";
      read_int ()
    with Failure _ -> 500 in
  let width = string_of_int (2 * iter - 1) in
  let height = string_of_int iter in
  Graphics.open_graph (" " ^ width ^ "x" ^ height);
  next_generation iter [Full];
  Graphics.read_key ()

;;

main ()
