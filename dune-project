(lang dune 3.14)
(name pars_comb_ml)
(generate_opam_files true)

(authors "Author Name")
(maintainers "Maintainer Name")
(license MIT)

(map_workspace_root false)

(package
 (name pars_comb_ml)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune (alcotest :with-test)))
