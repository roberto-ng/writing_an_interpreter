(lang dune 3.2)

(name writing_an_interpreter)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name writing_an_interpreter)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends 
  ocaml 
  dune 
  (batteries (= 3.7.1))
  (ppx_variants_conv (= v0.16.0)))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
