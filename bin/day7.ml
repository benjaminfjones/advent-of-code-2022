(*************
    Day 7
    ************)
open Core
open Aoc2022_lib

(* `entry` is a filesystem tree *)
type entry =
  | Dir of { name : string; entries : entry list }
  | File of { name : string; size : int }

let populate (e : entry) (stack : entry list) : entry =
  match e with
  | Dir { name; _ } -> Dir { name; entries = stack }
  | File _ -> failwith "cannot populate a File entry"

let rec entry_size (e : entry) : int =
  match e with
  | File { name = _; size } -> size
  | Dir { name = _; entries } -> sum_ints (List.map entries ~f:entry_size)

let rec alldirs e ~(f : entry -> bool) : entry list =
  match e with
  | File _ -> []
  | Dir { name = _; entries } ->
      let children = List.concat_map entries ~f:(alldirs ~f) in
      if f e then e :: children else children

let parse_entry line =
  match String.split line ~on:' ' with
  | [ "dir"; dirname ] -> Dir { name = dirname; entries = [] }
  | [ s; fn ] -> File { name = fn; size = int_of_string s }
  | _ -> failwith ("failed to parse entry line: " ^ line)

let pp_entry = function
  | Dir { name; _ } -> Format.sprintf "dir %s" name
  | File { name; size } -> Format.sprintf "%d %s" size name

(*
 * Branch type for the filesystem zipper
 *
 * A branch represents where we cd'd from and what the other entries at that
 * level of the filesystem there are. The zipper thread is a list (stack) of
 * branches.
 *)
type branch = CdFrom of { parent_name : string; other_entries : entry list }

let pp_branch = function
  | CdFrom { parent_name; other_entries } ->
      Format.sprintf "CdFrom(%s, %s)" parent_name
        (List.to_string other_entries ~f:pp_entry)

(* `filesystem` is an traversable filesystem tree.

   The filesystem is purely functional, immutable, supports incremental
   construction, and O(1) chdir. It is implemented using a zipper.
*)
type filesystem = { focus : entry; thread : branch list }

let pp_filesystem { focus; thread } =
  Format.sprintf "(%s, %s)" (pp_entry focus)
    (List.to_string thread ~f:pp_branch)

(* An empty root filesystem *)
let root_filesystem = { focus = Dir { name = "/"; entries = [] }; thread = [] }

let chdir_parent (fs : filesystem) : filesystem =
  match fs.thread with
  | [] -> failwith "cannot `cd ..`, already at root"
  | CdFrom { parent_name; other_entries } :: rest ->
      let focus' =
        Dir { name = parent_name; entries = fs.focus :: other_entries }
      in
      { focus = focus'; thread = rest }

let chdir_root (fs : filesystem) : filesystem =
  let rec aux f = if List.is_empty f.thread then f else aux (chdir_parent f) in
  aux fs

let chdir_deeper ~(target : string) (fs : filesystem) : filesystem =
  match fs.focus with
  | Dir { name = current_focus_name; entries = current_focus_entries } ->
      let select_target_entry e =
        match e with
        | Dir { name = ename; entries = _ } when String.equal target ename ->
            First e
        | _ -> Second e
      in
      let target_entries, other_entries =
        List.partition_map current_focus_entries ~f:select_target_entry
      in
      let focus' =
        match target_entries with
        | [] -> failwith ("dir does not contain " ^ target)
        | [ fcs ] -> fcs
        | _ -> failwith "dir contains multiple targets"
      in
      let new_branch =
        CdFrom { parent_name = current_focus_name; other_entries }
      in
      { focus = focus'; thread = new_branch :: fs.thread }
  | File _ -> raise @@ Failure "cannot chdir deeper from a file"

let chdir ~(target : string) (fs : filesystem) : filesystem =
  match target with
  | "/" -> chdir_root fs
  | ".." -> chdir_parent fs
  | _ -> chdir_deeper ~target fs

(* Commands start with $, e.g. `$ ls` and `$ cd foobar` *)
let is_command = String.is_prefix ~prefix:"$"

(* Listings do not start with a $ *)
let is_listing line = not (is_command line)

(* Build a filesystem incrementally by interpreting the given command lines *)
let interpret_cmds (lines : string list) : filesystem =
  let rec aux fs ls =
    match ls with
    | [] -> fs
    | line :: rest ->
        let fs', rest' =
          match String.split line ~on:' ' with
          | [] -> failwith "unexpected empty line"
          | [ "$"; "cd"; target ] -> (chdir ~target fs, rest)
          | [ "$"; "ls" ] ->
              let entries =
                List.map ~f:parse_entry (List.take_while rest ~f:is_listing)
              in
              let focus' = populate fs.focus entries in
              let fs' = { fs with focus = focus' } in
              let rest' = List.drop_while rest ~f:is_listing in
              (fs', rest')
          | _ -> failwith "unreachable"
        in
        aux fs' rest'
  in
  chdir ~target:"/" (aux root_filesystem lines)

let solve params lines =
  let fs = interpret_cmds lines in
  match params.(1) |> int_of_string with
  | 1 ->
      let size_upper_bound = 100000 in
      let filter e = entry_size e <= size_upper_bound in
      sum_ints (List.map (alldirs fs.focus ~f:filter) ~f:entry_size)
  | 2 -> (
      let total_fs_size = 70000000 in
      let needed_free_size = 30000000 in
      let used = entry_size fs.focus in
      let min_size_to_free = needed_free_size - (total_fs_size - used) in
      let filter e = entry_size e >= min_size_to_free in
      let candidates = List.map (alldirs fs.focus ~f:filter) ~f:entry_size in
      match List.min_elt candidates ~compare:Int.compare with
      | Some s -> s
      | None -> failwith "could not find directory big enough to delete!")
  | _ -> raise @@ Failure "invalid part"
