(lang dune 3.15)

(name miniml)

(generate_opam_files true)

(source
 (github seelx3/simple-prog-lang))

(authors "seelx3")

(maintainers "seelx3")

(license LICENSE)

; (documentation https://url/to/documentation)

(package
 (name miniml)
 (synopsis "miniml")
 (description "miniml a simple programming language")
 (depends ocaml dune)
 (tags
  (topics "ocaml")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
