#!/usr/bin/env bash

set -eux  # exit on error

opam switch create cs3110-2025sp ocaml-base-compiler.5.2.0
eval $(opam env)
opam install -y utop odoc ounit2 qcheck bisect_ppx menhir ocaml-lsp-server ocamlformat
# clean up caches and log files
opam clean
