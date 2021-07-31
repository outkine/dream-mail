(lang dune 2.7)
(name dream-mail)
(generate_opam_files true)

(package
 (name dream-mail)
 (synopsis "A library that adds email support to Dream")
 (depends
  (dune (>= "2.0"))
  (dream (>= "1.0.0~alpha2"))
  (emile (>= "1.1"))
  (lwt (>= "5.4"))
  (lwt.unix (>= "5.4"))
  (yojson (>= "1.7"))
  (ppx_yojson_conv (>= "1.0`"))))