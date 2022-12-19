open Core

(* tail of a list *)
let rec last = function
    | [] -> None
    | x :: [] -> Some x
    | _ :: rest -> last rest

(* let%test "last a b c d" = *)
(*     let r = last ["a"; "b"; "c"; "d"] in *)
(*     Option.equal String.equal r (Some "d") *)
(* let%test "last []" = *)
(*     let r = last [] in *)
(*     Option.equal String.equal r None *)


(* last two elements of a list *)
let rec last_two = function
    | [] -> None
    | _ :: [] -> None
    | x :: y :: [] -> Some (x, y)
    | _ :: rest -> last_two rest

(* let%test_unit "last_two 4-list" = *)
(*     [%test_eq: (string * string) option] *)
(*       (last_two ["a"; "b"; "c"; "d"]) *)
(*       (Some ("c", "d")) *)
(* let%test_unit "last_two singleton" = *)
(*     [%test_eq: (string * string) option] (last_two ["a"]) None *)

(* Find the N'th element of a list. *)
let rec nth xs n =
    match (xs, n) with
    | ([], _) -> raise (Failure "empty list")
    | (_, n) when n < 0 -> raise (Failure "negative number")
    | (x :: _, 0) -> x
    | (_ :: rest, n) -> nth rest (n - 1)
(* let%test_unit "nth second of 5" = *)
(*     [%test_eq: string] (nth ["a"; "b"; "c"; "d"; "e"] 2) "c" *)
(* let%test "nth second of 1" = *)
(*     let ignore_str _ = false in *)
(*     try ignore_str (nth ["a"] 2) with *)
(*     | Failure _ -> true *)
(*     | _ -> false *)
