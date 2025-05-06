(lang dune 3.17)

(name resume)

(generate_opam_files true)

(source
 (github cesardeazevedo/resume))

(authors "Cesar Augusto <cesardeazevedo@outlook.com>")

(maintainers "Cesar Augusto <cesardeazevedo@outlook.com>")

(license MIT)

(documentation https://github.com/cesardeazevedo/resume)

(dialect
 (name mlx)
 (implementation
  (extension mlx)
  (merlin_reader mlx)
  (preprocess
   (run mlx-pp %{input-file}))))

(package
 (name resume)
 (synopsis "Resume builder")
 (allow_empty)
 (description "Resume builder")
 (depends ocaml dune melange reason-react mlx ocamlmerlin-mlx)
 (tags ("resume")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
