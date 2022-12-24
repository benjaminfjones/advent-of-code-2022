open Core
open Lib.Util

(*
 * Expressions and expression comparison
 *)

type expr = EList of expr list | ENum of int

(* expression comparison, see https://adventofcode.com/2022/day/13 *)
let rec expr_compare e1 e2 =
  match (e1, e2) with
  | ENum n, ENum m -> Int.compare n m
  | ENum _, EList _ -> expr_compare (EList [ e1 ]) e2
  | EList _, ENum _ -> expr_compare e1 (EList [ e2 ])
  | EList [], EList [] -> 0
  | EList [], EList _ -> -1
  | EList (_ :: _), EList [] -> 1
  | EList (f1 :: rest1), EList (f2 :: rest2) ->
      let fst_comparison = expr_compare f1 f2 in
      if fst_comparison = 0 then expr_compare (EList rest1) (EList rest2)
      else fst_comparison

(* yep *)
type token = string

let lexer (input : string) : token list =
  let rec loop chars =
    match chars with
    | [] -> []
    | c :: cs when Char.is_whitespace c -> loop cs
    | c :: cs when Char.is_digit c ->
        let num = c :: List.take_while cs ~f:Char.is_digit in
        let rest = List.drop_while cs ~f:Char.is_digit in
        String.of_char_list num :: loop rest
    | '[' :: cs -> "[" :: loop cs
    | ',' :: cs -> "," :: loop cs
    | ']' :: cs -> "]" :: loop cs
    | _ -> failwith "syntax error"
  in
  loop (String.to_list input)

(* Lester-Jones style parser combinator type *)
type 'a parser = token list -> 'a * token list list

let take_fst x _ = x
let take_snd _ y = y

(* parser an integer *)
let p_int toks =
  match toks with
  | [] -> []
  | s :: rest -> ( try [ (int_of_string s, rest) ] with _ -> [])

(* parser a literal string *)
let p_lit l toks =
  match toks with t :: ts when String.equal t l -> [ (l, ts) ] | _ -> []

(* parse either `p1` or `p2`; returns all parses *)
let p_alt p1 p2 toks = List.append (p1 toks) (p2 toks)

let p_then_with_parses ~combine ps p2 =
  List.concat_map ps ~f:(fun (a, toks1) ->
      List.map (p2 toks1) ~f:(fun (b, toks2) -> (combine a b, toks2)))

(* sequence two parsers *)
let p_then ~combine p1 p2 toks = p_then_with_parses ~combine (p1 toks) p2

(* matches any token stream, consumed nothing, and returns the given value *)
let p_empty a toks = [ (a, toks) ]

(* greedy zero or more
 *
 * Note: implementing `p_zero_or_more` and `p_one_or_more` mutually recursive
 * and using `p_alt` with `p_empty []` does not work (leads to infinite
 * recursion) without inserting thunks strategically.
 *)
let rec p_zero_or_more p toks =
  match p toks with
  | [] -> [ ([], toks) ]
  | ps -> p_then_with_parses ~combine:List.cons ps (p_zero_or_more p)

(* greedy one or more *)
let p_one_or_more p = p_then ~combine:List.cons p (p_zero_or_more p)

(* greedy zero or more with separator *)
let p_zero_or_more_sep p sep toks =
  let sep_then_p = p_then ~combine:take_snd sep p in
  match p toks with
  | [] -> [ ([], toks) ]
  | ps -> p_then_with_parses ~combine:List.cons ps (p_zero_or_more sep_then_p)

(* greedy one or more with separator *)
let p_one_or_more_sep p sep =
  let sep_then_p = p_then ~combine:take_snd sep p in
  p_then ~combine:List.cons p (p_zero_or_more sep_then_p)

let p_apply p ~f toks = List.map (p toks) ~f:(fun (a, ts) -> (f a, ts))

let p_bracketed p =
  p_then ~combine:take_snd (p_lit "[") (p_then ~combine:take_fst p (p_lit "]"))

let p_enum = p_apply ~f:(fun x -> ENum x) p_int

let rec p_elist toks =
  let pp =
    p_apply
      ~f:(fun xs -> EList xs)
      (p_bracketed (p_zero_or_more_sep p_expr (p_lit ",")))
  in
  pp toks

and p_expr toks = p_alt p_enum p_elist toks

(* top-level expression parser *)
let parse_elist (s : string) : expr =
  let toks = lexer s in
  match p_elist toks with
  | [] -> failwith ("failed to parse: " ^ s)
  | (l, _) :: _ -> l

(*
 * Input processing
 *)

let parse_lines lines =
  lines
  |> List.filter ~f:(fun s -> not (String.is_empty s))
  |> List.map ~f:parse_elist

let solve params lines =
  let packets = parse_lines lines in
  match params.(1) |> int_of_string with
  | 1 ->
      (* sum indices of packet pairs that are in the correct order *)
      let packet_pairs = List.chunks_of ~length:2 packets in
      let compare_pair = function
        | [ e1; e2 ] -> expr_compare e1 e2
        | _ -> failwith "unmatched pair"
      in
      let index_if_correct_order i p =
        if compare_pair p < 0 then Some (i + 1) else None
      in
      List.filter_mapi ~f:index_if_correct_order packet_pairs |> sum_ints
  | 2 ->
      (* divider packets *)
      let dp1 = EList [ EList [ ENum 2 ] ] in
      let dp2 = EList [ EList [ ENum 6 ] ] in
      let packets = dp1 :: dp2 :: packets in
      let sorted_packets = List.sort packets ~compare:expr_compare in
      let idx1 =
        List.findi_exn sorted_packets ~f:(fun _ e -> expr_compare e dp1 = 0)
      in
      let idx2 =
        List.findi_exn sorted_packets ~f:(fun _ e -> expr_compare e dp2 = 0)
      in
      (fst idx1 + 1) * (fst idx2 + 1)
  | _ -> raise @@ Failure "invalid part"
