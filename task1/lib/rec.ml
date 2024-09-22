let is_good x = x mod 3 == 0 || x mod 5 == 0

let rec sum_3_5 x =
  if x > 0 then sum_3_5 (x - 1) + if is_good x then x else 0 else 0
