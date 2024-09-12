let is_good x = x mod 3 == 0 || x mod 5 == 0
let generate_range n = List.init n (fun x -> x + 1)

let sum_of_3_5 n =
  generate_range n
  |> List.map (fun x -> if is_good x then x else 0)
  |> List.fold_left ( + ) 0

(** TESTS *)

let%test "problem" = 23 == sum_of_3_5 9
let%test "solution" = 233168 == sum_of_3_5 999
