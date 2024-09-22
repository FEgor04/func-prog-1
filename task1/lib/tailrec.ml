let is_good x = x mod 3 == 0 || x mod 5 == 0

let rec sum_3_5_tail x acc =
  if x > 0 then sum_3_5_tail (x - 1) (acc + if is_good x then x else 0) else acc

let sum_3_5 x = sum_3_5_tail x 0
let%test _ = sum_3_5 9 = 23
let%test _ = sum_3_5 999 = 233168
