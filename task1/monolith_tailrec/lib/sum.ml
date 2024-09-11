let rec tail_sum_of_3_5 acc x =
  if x > 0 then
    if x mod 3 == 0 || x mod 5 == 0 then tail_sum_of_3_5 (acc + x) (x - 1)
    else tail_sum_of_3_5 acc (x - 1)
  else acc

let%test "problem" = 23 == tail_sum_of_3_5 0 9
let%test "solution" = 233168 == tail_sum_of_3_5 0 999
