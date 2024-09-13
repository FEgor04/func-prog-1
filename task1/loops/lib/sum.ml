let answer = ref 0
let is_good x = x mod 3 == 0 || x mod 5 == 0
let nullify_answer () = answer := 0

let sum_of_3_5 n =
  nullify_answer ();
  for i = 1 to n do
    answer := !answer + if is_good i then i else 0
  done;
  !answer

let%test "problem" = 23 == sum_of_3_5 9
let%test "solution" = 233168 == sum_of_3_5 999
