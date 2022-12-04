# advent-of-code-2022

## Setting up OCaml development environment

See https://ocaml.org/docs/up-and-running

Install and setup the default switch with toplevels, compilers, LSP, and build
system:

```
brew install opam
opam init
echo '# OCaml' >> ~/.zshrc_local
echo '[[ ! -r /Users/bfj/.opam/opam-init/init.zsh ]] || source /Users/bfj/.opam/opam-init/init.zsh  > /dev/null 2> /dev/null' >> ~/.zshrc_local
eval $(opam env --switch=default)
opam install dune merlin ocaml-lsp-server odoc ocamlformat utop dune-release core core_bench
```

## Day Solution Structure

Basic project layout adapted from https://github.com/shonfeder/aoc2021,
and is subject to radical change.

There is one dune project with simple top-level `dune-project` file.

### Day solutions

There is one module under `bin/` for each day. Each day module implements the
`Solver` module type. Day modules are selected and instantiated in
`bin/main.ml` which also feeds params (part #, etc) and input from `stdin`.

```ocaml
let solve params _lines =
  (* let domain = parse_input lines in *)
  match params.(1) |> int_of_string with
  | 1 -> 0 (* TODO solve part 1 *)
  | 2 -> 0 (* TODO solve part 2 *)
  | _ -> raise @@ Failure "invalid part"
```

### Common code

Common code goes in `lib/aoc2022_lib.ml`, which also defines the `Solver`
module type.

### Tests

All solutions for test inputs and both problems parts are implemented as cram
tests. For example, day 1 tests live under `test/day1.t`.

### TODO Each Day

1. create new `bin/dayN.ml` implementing `Solver`
2. add module `DayN` to list in `bin/main.ml`
3. setup cram test dir for day N:

```shell
export AOC_DAY=N
mkdir -p test/day${AOC_DAY}.t
cat <<EOF >test/day${AOC_DAY}.t/run.t
https://adventofcode.com/2022/day/${AOC_DAY}

  $ export AOC_DAY=${AOC_DAY}

Part 1: TBD

  $ aoc2022 1 < test
  0

  $ aoc2022 1 < input
  0

Part 2: TBD

  $ aoc2022 2 < test
  0

  $ aoc2022 2 < input
  0
EOF
touch test/day${AOC_DAY}.t/test
touch test/day${AOC_DAY}.t/input
```