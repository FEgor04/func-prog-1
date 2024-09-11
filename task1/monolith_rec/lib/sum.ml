let rec sum_of_3_5 x =
  if x > 0 then
    sum_of_3_5 (x - 1) + if x mod 3 == 0 || x mod 5 == 0 then x else 0
  else 0

let%test "problem" = 23 == sum_of_3_5 9
let%test "solution" = 233168 == sum_of_3_5 999
