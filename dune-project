(lang dune 3.7)
(using ctypes 0.1)

(generate_opam_files true)

(name taype-drivers)
(license "MIT")
(maintainers "Qianchuan Ye <yeqianchuan@gmail.com>")
(authors "Qianchuan Ye")
(source (github ccyip/taype-drivers))

(package
 (name taype-driver-lib)
 (synopsis "Common types and routines for writing drivers for the Taype language")
)

(package
 (name taype-driver-plaintext)
 (synopsis "Plaintext driver for the Taype language")
)

(package
 (name taype-driver-emp)
 (depends (ctypes (>= 0.20)))
 (synopsis "Emp-toolkit driver for the Taype language")
)
