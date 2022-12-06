open Core

(* given a list of chars, determine if all are distinct *)
let distinct xs = not (List.contains_dup ~compare:Char.compare xs)

(* Return a list of sliding windows of the given positive length *)
let sliding_windows ~(length : int) (xs : 'a list) : 'a list list =
  let () = assert (length > 0) in
  let rec aux result xs =
    if List.length xs >= length then
      let window = List.take xs length in
      aux (window :: result) (List.drop xs 1)
    else result
  in
  List.rev (aux [] xs)

(* Return the end position of the first length `length` subsequence of distinct chars *)
let pos_of_first_unique ~(length : int) (xs : char list) : int =
  let enum_windows =
    List.mapi (sliding_windows ~length xs) ~f:(fun i x -> (x, i))
  in
  match List.find enum_windows ~f:(fun (w, _i) -> distinct w) with
  | Some (_w, i) -> i + length
  | None -> raise @@ Failure "failed to find marker"

let solve params lines =
  let datastream = String.to_list (List.hd_exn lines) in
  match params.(1) |> int_of_string with
  | 1 -> pos_of_first_unique ~length:4 datastream
  | 2 -> pos_of_first_unique ~length:14 datastream
  | _ -> raise @@ Failure "invalid part"