let answer = ref 0
let rec pow x n = if n > 1 then pow x (n - 1) * x else x

let rec number_to_digits x =
  (x mod 10) :: (if x / 10 > 0 then number_to_digits (x / 10) else [])

let sum_of_powers xs power =
  xs |> List.map (fun x -> pow x power) |> List.fold_left ( + ) 0

let is_good x power = sum_of_powers (number_to_digits x) power == x
let nullify_answer () = answer := 0

let sum n =
  nullify_answer ();
  for i = 2 to n * pow 9 n do
    answer := !answer + if is_good i n then i else 0
  done;
  !answer

let%test _ = sum 4 == 19316
let%test _ = sum 5 == 443839
