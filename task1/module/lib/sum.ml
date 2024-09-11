let is_even x y = x mod y == 0
let is_good x = is_even x 5 || is_even x 3
let generate_range n = List.init n (fun x -> x + 1)
let filter_good = List.filter is_good
let list_sum = List.fold_left ( + ) 0
let sum_of_3_5 n = generate_range n |> filter_good |> list_sum
let%test "problem" = 23 == sum_of_3_5 9
let%test "solution" = 233168 == sum_of_3_5 999
