let is_good x = x mod 3 == 0 || x mod 5 == 0

let rec sum_3_5 x =
  match x with
  | 0 -> 0
  | x when is_good x -> sum_3_5 (x - 1) + x
  | x when not (is_good x) -> sum_3_5 (x - 1)
  | _ -> 0
;;

let%test _ = sum_3_5 9 = 23
let%test _ = sum_3_5 999 = 233168
