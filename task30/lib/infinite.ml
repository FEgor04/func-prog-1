let rec list_last lst =
  match lst with [] -> None | [ tl ] -> Some tl | _ :: tl -> list_last tl

let sequence_at n s = s |> Seq.take n |> List.of_seq |> list_last
let pows n = Seq.unfold (fun x -> Some (x, x * n)) n
let pow x n = pows x |> sequence_at n |> Option.get

let rec number_to_digits x =
  (x mod 10) :: (if x / 10 > 0 then number_to_digits (x / 10) else [])

let rec sum_of_powers xs power =
  match xs with [] -> 0 | x :: xs -> pow x power + sum_of_powers xs power

let is_good power x = sum_of_powers (number_to_digits x) power == x
let can_be_good power x = x < pow 9 power * power

let good_numbers power =
  Seq.unfold (fun x -> if can_be_good power x then Some (x, x + 1) else None) 2
  |> Seq.filter (is_good power)

let sum power = good_numbers power |> Seq.fold_left ( + ) 0
let%test _ = sum 4 == 19316
let%test _ = sum 5 == 443839
