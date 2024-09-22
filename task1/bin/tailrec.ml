let is_good x = x mod 3 == 0 || x mod 5 == 0

let rec sum_3_5 x acc =
  if x > 0 then sum_3_5 (x - 1) (acc + if is_good x then x else 0) else acc

let ans = sum_3_5 999 0
let () = print_endline (string_of_int ans)
