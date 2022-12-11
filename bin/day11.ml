open Core

type monkey = {
  id : int;
  items : int list;
  operation : int -> int;
  test : int -> int;
  num_inspect : int;
}

type throw = { dest : int; item : int }

let take_turn (mky : monkey) ~(relief : int -> int) : monkey * throw list =
  let inspect item =
    (item |> mky.operation |> relief |> fun x -> (mky.test x, x))
    |> fun (dest, x) -> { dest; item = x }
  in
  ( {
      mky with
      items = [];
      num_inspect = mky.num_inspect + List.length mky.items;
    },
    List.map ~f:inspect mky.items )

let monkey_throws (mkys : monkey list) (ts : throw list) : monkey list =
  let throw1 ms t =
    ms
    |> List.map ~f:(fun m ->
           if m.id = t.dest then
             { m with items = List.append m.items [ t.item ] }
           else m)
  in
  List.fold ts ~init:mkys ~f:(fun acc t -> throw1 acc t)

let list_replace_exn (xs : 'a list) ~(pos : int) (y : 'a) : 'a list =
  let prefix, suffix = List.split_n xs pos in
  List.concat [ prefix; [ y ]; List.tl_exn suffix ]

let play_round (mkys : monkey list) ~(relief : int -> int) : monkey list =
  let mky_ids = List.map mkys ~f:(fun m -> m.id) in
  List.fold mky_ids ~init:mkys ~f:(fun acc_mkys cur_mky_id ->
      let cur_mky = List.find_exn acc_mkys ~f:(fun m -> m.id = cur_mky_id) in
      let cur_mky', throws = take_turn cur_mky ~relief in
      (* replace cur_mky's items with [] *)
      let acc_mkys' = list_replace_exn acc_mkys ~pos:cur_mky_id cur_mky' in
      (* add cur_mky's thrown items to their destination monkey *)
      monkey_throws acc_mkys' throws)

let test_monkies =
  [
    {
      id = 0;
      items = [ 79; 98 ];
      operation = (fun x -> x * 19);
      test = (fun x -> if x % 23 = 0 then 2 else 3);
      num_inspect = 0;
    };
    {
      id = 1;
      items = [ 54; 65; 75; 74 ];
      operation = (fun x -> x + 6);
      test = (fun x -> if x % 19 = 0 then 2 else 0);
      num_inspect = 0;
    };
    {
      id = 2;
      items = [ 79; 60; 97 ];
      operation = (fun x -> x * x);
      test = (fun x -> if x % 13 = 0 then 1 else 3);
      num_inspect = 0;
    };
    {
      id = 3;
      items = [ 74 ];
      operation = (fun x -> x + 3);
      test = (fun x -> if x % 17 = 0 then 0 else 1);
      num_inspect = 0;
    };
  ]

(* These structs are the result of a vim macro over the original input :)
 *
 * See `day11_vim_macro.txt`
 *
 *   Monkey 0:                                { id = 0;
 *     Starting items: 83, 97, 95, 67         items = [ 83; 97; 95; 67 ];
 *     Operation: new = old * 19              operation = (fun old -> old * 19);
 *     Test: divisible by 17           ==>    test = (fun x -> if x % 17 = 0 then
 *       If true: throw to monkey 2             then 2
 *       If false: throw to monkey 7            else 7) };
 *)

let input_monkeys =
  [
    {
      id = 0;
      items = [ 83; 97; 95; 67 ];
      operation = (fun old -> old * 19);
      test = (fun x -> if x % 17 = 0 then 2 else 7);
      num_inspect = 0;
    };
    {
      id = 1;
      items = [ 71; 70; 79; 88; 56; 70 ];
      operation = (fun old -> old + 2);
      test = (fun x -> if x % 19 = 0 then 7 else 0);
      num_inspect = 0;
    };
    {
      id = 2;
      items = [ 98; 51; 51; 63; 80; 85; 84; 95 ];
      operation = (fun old -> old + 7);
      test = (fun x -> if x % 7 = 0 then 4 else 3);
      num_inspect = 0;
    };
    {
      id = 3;
      items = [ 77; 90; 82; 80; 79 ];
      operation = (fun old -> old + 1);
      test = (fun x -> if x % 11 = 0 then 6 else 4);
      num_inspect = 0;
    };
    {
      id = 4;
      items = [ 68 ];
      operation = (fun old -> old * 5);
      test = (fun x -> if x % 13 = 0 then 6 else 5);
      num_inspect = 0;
    };
    {
      id = 5;
      items = [ 60; 94 ];
      operation = (fun old -> old + 5);
      test = (fun x -> if x % 3 = 0 then 1 else 0);
      num_inspect = 0;
    };
    {
      id = 6;
      items = [ 81; 51; 85 ];
      operation = (fun old -> old * old);
      test = (fun x -> if x % 5 = 0 then 5 else 1);
      num_inspect = 0;
    };
    {
      id = 7;
      items = [ 98; 81; 63; 65; 84; 71; 84 ];
      operation = (fun old -> old + 3);
      test = (fun x -> if x % 2 = 0 then 2 else 3);
      num_inspect = 0;
    };
  ]

let compute_monkey_business mkys =
  let total_inspections = List.map mkys ~f:(fun m -> m.num_inspect) in
  let sorted = List.sort total_inspections ~compare:Int.descending in
  List.nth_exn sorted 0 * List.nth_exn sorted 1

let solve params _lines =
  let the_monkies = input_monkeys in
  (* reduce worrry level by factor of 3, rounded down *)
  let p1_relief x = x / 3 in
  (* reduce worrry level mod P, the product of all test modulous's. If
     the worry level is 0 mod P, then it is 0 mod t for all monkey's tests. *)
  let p2_test_relief x = x % (13 * 17 * 19 * 23) in
  let p2_input_relief x = x % (2 * 3 * 5 * 7 * 11 * 13 * 17 * 19) in
  match params.(1) |> int_of_string with
  (* part 1, test *)
  | 1 ->
      let round20 =
        List.fold (List.range 0 20) ~init:test_monkies ~f:(fun acc _ ->
            play_round acc ~relief:p1_relief)
      in
      compute_monkey_business round20
  (* part 1, input *)
  | 2 ->
      let round20 =
        List.fold (List.range 0 20) ~init:the_monkies ~f:(fun acc _ ->
            play_round acc ~relief:p1_relief)
      in
      compute_monkey_business round20
  | 3 ->
      let round10k =
        List.fold (List.range 0 10000) ~init:test_monkies ~f:(fun acc _ ->
            play_round acc ~relief:p2_test_relief)
      in
      compute_monkey_business round10k
  | 4 ->
      let round10k =
        List.fold (List.range 0 10000) ~init:the_monkies ~f:(fun acc _ ->
            play_round acc ~relief:p2_input_relief)
      in
      compute_monkey_business round10k
  | _ -> raise @@ Failure "invalid part"