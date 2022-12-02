(lang dune 3.6)

(name advent_of_code_2022)

(generate_opam_files true)

(cram enable)

(source
 (github benjaminfjones/advent-of-code-2022))

(authors "Benjamin F Jones")

(maintainers "Benjamin F Jones")

(license LICENSE)

(documentation https://github.com/benjaminfjones/advent-of-code-2022)

(package
 (name advent_of_code_2022)
 (synopsis "Advent of Code 2022 Solutions in OCaml")
 (description "Advent of Code 2022 Solutions in OCaml")
 (depends
   ocaml
   dune
   ppx_inline_test
   core)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
