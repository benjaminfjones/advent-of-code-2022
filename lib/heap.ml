open Core

(* `Node`s form an n-ary Pairing Tree *)
module Node = struct
  type 'a t = { value : 'a; children : 'a t list }
end

open Node

type 'a t = { compare : 'a -> 'a -> int; length : int; heap : 'a Node.t option }

let empty ~compare = { compare; length = 0; heap = None }

(* Merge two Pairing Trees *)
let merge ~compare ({ value = e1; children = nl1 } as n1)
    ({ value = e2; children = nl2 } as n2) =
  if compare e1 e2 < 0 then { value = e1; children = n2 :: nl1 }
  else { value = e2; children = n1 :: nl2 }

(* Merge a list of Pairing Trees in pairs left-to-right, then fold back
   right to left *)
let merge_pairs ~(compare : 'a -> 'a -> int) (trees : 'a Node.t list) =
  let rec loop acc ts =
    match ts with
    | [] -> acc
    | [ head ] -> head :: acc
    | head :: next1 :: next2 -> loop (merge ~compare head next1 :: acc) next2
  in
  match loop [] trees with
  | [] -> None
  | [ h ] -> Some h
  | x :: xs -> Some (List.fold xs ~init:x ~f:(merge ~compare))

let add { compare; length; heap } e =
  let new_node = { value = e; children = [] } in
  let heap' =
    match heap with
    | None -> new_node
    | Some heap -> merge ~compare new_node heap
  in
  { compare; length = length + 1; heap = Some heap' }

let top_exn t =
  match t.heap with
  | None -> failwith "top_exn called on an empty heap"
  | Some { value; _ } -> value

let top t = try Some (top_exn t) with _ -> None

let pop_exn { compare; length; heap } =
  match heap with
  | None -> failwith "pop_exn called on an empty heap"
  | Some { value; children } ->
      let new_heap = merge_pairs ~compare children in
      let h' = { compare; length = length - 1; heap = new_heap } in
      (value, h')

let pop t = try Some (pop_exn t) with _ -> None
let remove_top_exn t = snd (pop_exn t)

let remove_top t =
  try
    let _, t' = pop_exn t in
    Some t'
  with _ -> None

let of_list xs ~compare =
  List.fold xs ~init:(empty ~compare) ~f:(fun h x -> add h x)

let to_list h =
  let rec aux result t =
    match pop t with None -> result | Some (x, t') -> aux (x :: result) t'
  in
  aux [] h
