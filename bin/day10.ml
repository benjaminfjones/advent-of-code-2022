open Core

type instruction = Noop | Addx of int

let parse_line (line : string) : instruction =
  match String.split line ~on:' ' with
  | [ "noop" ] -> Noop
  | [ "addx"; n ] -> Addx (int_of_string n)
  | _ -> failwith ("failed to parse: " ^ line)

type state = {
  (* start of the indicated cycle *)
  cycle : int;
  (* valud of x at the start of the cycle *)
  x : int;
  (* probes to check at the start of each cycle *)
  probe_at_cycle : int list;
  (* trace of probe values (cycle, x value) *)
  probe_values : (int * int) list;
  (* CRT display buffer *)
  crt : bool list;
}

let initial_state probes =
  { cycle = 1; x = 1; probe_at_cycle = probes; probe_values = []; crt = [] }

let run_instruction (st : state) (inst : instruction) : state =
  let cycle', x' =
    match inst with
    | Noop -> (st.cycle + 1, st.x)
    | Addx n -> (st.cycle + 2, st.x + n)
  in
  let triggered_probes =
    List.filter st.probe_at_cycle ~f:(fun pt -> pt > st.cycle && pt <= cycle')
  in
  let probe_values' =
    List.append
      (List.rev
         (List.map
            ~f:(fun pt -> if pt < cycle' then (pt, st.x) else (pt, x'))
            triggered_probes))
      st.probe_values
  in
  let crt_update =
    List.range 0 (cycle' - st.cycle)
    |> List.map ~f:(fun dt -> Int.abs (st.x - ((st.cycle + dt - 1) % 40)) < 2)
    |> List.rev
  in
  {
    cycle = cycle';
    x = x';
    probe_at_cycle = st.probe_at_cycle;
    probe_values = probe_values';
    crt = List.append crt_update st.crt;
  }

let run_all_instructions (insts : instruction list) (st : state) : state =
  List.fold insts ~init:st ~f:run_instruction

let signal_strength (probe_values : (int * int) list) : int =
  List.fold probe_values ~init:0 ~f:(fun acc (cycle, value) ->
      acc + (cycle * value))

let display_crt crt =
  let pixel_of_bool b = if b then "#" else "." in
  List.rev crt
  |> List.mapi ~f:(fun i v ->
         let p = pixel_of_bool v in
         if i % 40 = 39 then p ^ "\n" else p)
  |> String.concat

let solve params lines =
  let insts = List.map ~f:parse_line lines in
  let probes = [ 20; 60; 100; 140; 180; 220 ] in
  match params.(1) |> int_of_string with
  | 1 ->
      let st0 = initial_state probes in
      let final_st = run_all_instructions insts st0 in
      signal_strength final_st.probe_values
  | 2 ->
      let st0 = initial_state [] in
      let final_st = run_all_instructions insts st0 in
      Printf.printf "%s" (display_crt final_st.crt);
      0
  | _ -> raise @@ Failure "invalid part"
