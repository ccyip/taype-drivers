# Taype Drivers

Plaintext and EMP drivers for the Taype language.

## Dependencies

To build this library, we need the OCaml toolchain, managed by
[opam](https://opam.ocaml.org), and the dependencies for [EMP
toolkit](https://github.com/emp-toolkit/emp-tool), notably OpenSSL.

After setting up opam and OCaml (currently best-tested on 4.14.1), install the
OCaml dependencies by:

``` sh
opam install dune containers ctypes
```

## Build and install

First, build and install EMP toolkit and `emp-ffi` (FFI bindings for our OCaml
library). You may have to explicitly provide OpenSSL path. To avoid polluting
our system, here we will install the libraries to `$OPAM_SWITCH_PREFIX`, so we
can simply remove the opam switch when we are done with it. But you can also
install them directly to a system prefix (e.g., when you install them in a
Docker container or virtual machine).

``` sh
cd emp/ffi
make PREFIX="$OPAM_SWITCH_PREFIX"
```

Now we build and install the drivers. You may have to set the environment
variable of library path if the previous libraries were not installed to a
system prefix. This environment variable also needs to be set to run the
compiled Taype programs.

``` sh
# On Linux
export LD_LIBRARY_PATH "$OPAM_SWITH_PREFIX/lib"

# On Mac
export DYLD_LIBRARY_PATH "$OPAM_SWITH_PREFIX/lib"

dune build
dune install
```
