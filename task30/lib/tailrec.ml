let rec tailpow x n acc = if n > 1 then tailpow x (n - 1) (acc * x) else acc * x
let pow x n = tailpow x n 1
let%test _ = pow 2 1 == 2
let%test _ = pow 2 2 == 4
let%test _ = pow 2 3 == 8

let rec tail_number_to_digits x acc =
  if x / 10 > 0 then tail_number_to_digits (x / 10) ((x mod 10) :: acc)
  else (x mod 10) :: acc

let number_to_digits x = tail_number_to_digits x []
let%test _ = List.equal ( == ) (number_to_digits 1) [ 1 ]
let%test _ = List.equal ( == ) (number_to_digits 12) [ 1; 2 ]
let%test _ = List.equal ( == ) (number_to_digits 123) [ 1; 2; 3 ]

let rec tail_sum_of_powers xs power acc =
  match xs with
  | [] -> acc
  | x :: xs -> tail_sum_of_powers xs power (acc + pow x power)

let sum_of_powers xs power = tail_sum_of_powers xs power 0
let%test _ = sum_of_powers [ 1 ] 4 == 1
let%test _ = sum_of_powers [ 1; 2 ] 4 == 1 + 16
let is_good x power = sum_of_powers (number_to_digits x) power == x
let%test _ = is_good 1634 4
let%test _ = is_good 8208 4
let%test _ = is_good 9474 4

let rec tail_sum_less_than x power acc =
  if x > 1 then
    tail_sum_less_than (x - 1) power (acc + if is_good x power then x else 0)
  else acc

let sum_less_than x power = tail_sum_less_than x power 0
let sum power = sum_less_than (pow 9 power * power) power
let%test _ = sum 4 == 19316
let%test _ = sum 5 == 443839
