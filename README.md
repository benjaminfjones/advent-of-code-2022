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

Subject to radical change

* one dune project with simple top-level `dune-project` file
* one directory for each day with:
  - `dune` file specifying name and deps

```
(executable
 (public_name day01)
 (name main)
 (libraries core))
```

  - one `.ml` file with solution
  - inline tests for components
  - expect tests?
