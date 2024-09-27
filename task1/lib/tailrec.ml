let is_good x = x mod 3 == 0 || x mod 5 == 0

let rec sum_3_5_tail x acc =
  match x with
  | 0 -> acc
  | x when is_good x -> sum_3_5_tail (x - 1) (acc + x)
  | x when not (is_good x) -> sum_3_5_tail (x - 1) acc
  | _ -> acc
;;

let sum_3_5 x = sum_3_5_tail x 0
let%test _ = sum_3_5 9 = 23
let%test _ = sum_3_5 999 = 233168
