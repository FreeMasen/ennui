(lang dune 3.18)

(name ennui)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name ennui)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml)
 (depopts ounit2)
 (tags
  ("lua" "tokenizer", "lexer", "lex")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
