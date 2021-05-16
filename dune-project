(lang dune 2.7)
(generate_opam_files true)

(source (github rbardou/mysc))
(license MIT)
(authors "Romain Bardou")
(maintainers "Romain Bardou")

(package
 (name mysc)
 (synopsis "Very small extension to the standard library")
 (description "This library defines functions that I tend to use in most of my projects.")
 (depends (ocaml (>= 4.07))))
