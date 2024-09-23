let is_good x = x mod 3 == 0 || x mod 5 == 0
let generate_sequence n = List.init n (fun x -> x + 1)

let sum_3_5 n =
  generate_sequence n |> List.filter is_good |> List.fold_left ( + ) 0

let%test _ = sum_3_5 9 = 23
let%test _ = sum_3_5 999 = 233168