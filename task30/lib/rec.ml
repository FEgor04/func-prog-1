let rec pow x n = if n > 1 then pow x (n - 1) * x else x

let%test _ = pow 2 1 == 2
let%test _ = pow 2 2 == 4
let%test _ = pow 2 3 == 8

let rec number_to_digits x = x mod 10 :: if (x/10) > 0 then number_to_digits (x / 10) else []

let%test _ = List.equal (==) (number_to_digits 1) [1]
let%test _ = List.equal (==) (number_to_digits 12) [2;1]
let%test _ = List.equal (==) (number_to_digits 123) [3;2;1]

let sum_of_powers xs power =
  List.map (fun x -> pow x power) xs |> List.fold_left ( + ) 0

let%test _ = sum_of_powers [1] 4 == 1
let%test _ = sum_of_powers [1;2] 4 == 1 + 16

let is_good x power = sum_of_powers (number_to_digits x) power == x

let%test _ = is_good 1634 4
let%test _ = is_good 8208 4
let%test _ = is_good 9474 4

let rec sum_less_than x power =
  if x > 1 then sum_less_than (x - 1) power + if is_good x power then x else 0
  else 0

let sum power = sum_less_than (pow 9 (power) * power) power

let%test _ = sum 4 == 19316
let%test _ = sum 5 == 443839
