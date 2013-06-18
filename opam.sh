#!/bin/sh

git clone https://github.com/OCamlPro/opam.git
(cd opam; git checkout 1.0.0; ./configure --prefix /app/vendor/opam; make; make install)
tar zcvf opam.tgz -C /app vendor/opam

export PATH=/app/vendor/opam/bin:$PATH

opam init -y
eval `opam config env`
