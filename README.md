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

### Common code

Common code goes in `lib/aoc2022_lib.ml`, which also defines the `Solver`
module type.

### Tests

All solutions for test inputs and both problems parts are implemented as cram
tests. For example, day 1 tests live under `test/day1.t`.
