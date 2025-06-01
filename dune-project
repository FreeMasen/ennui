(lang dune 3.18)

(name ennui)

(generate_opam_files true)

(source
 (github FreeMasen/ennui))

(authors "Robert Masen <r.f.masen@gmail.com>")

(maintainers "Robert Masen <r.f.masen@gmail.com>")

(license MIT)

(documentation https://github.com/FreeMasen/ennui)

(package
 (name ennui)
 (synopsis "Lexer for Lua in ocaml")
 (description "Lexer for Lua in ocaml")
 (depends ocaml)
 (tags
  ("lua" "tokenizer", "lexer", "lex")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
