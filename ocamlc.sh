#!/bin/sh

#heroku run bash 

set -e

curl -o custom.tar.bz2 https://bitbucket.org/camlspotter/mutated_ocaml/get/custom.tar.bz2
tar jxvf cusotm.tar.bz2
cd camlspotter-mutated_ocaml-*
./configure --prefix /app/vendor/ocaml
make core coreboot world opt opt.opt install
tar zcvf ocaml-4.01.0-bin.tgz vendor/ocaml
scp -P 11112 ocaml-4.01.0-bin.tgz jun@49.212.130.159:

export PATH=/app/vendor/ocaml/bin:$PATH

cd
curl -o opam-full-1.1.0.tar.gz http://www.ocamlpro.com/pub/opam-full-1.1.0.tar.gz
tar zxvf opam-full-1.1.0.tar.gz
cd opam-full-1.1.0
./configure --prefix /app/vendor/opam
cd
tar zcvf opam-1.1.0-bin.tgz vendor/opam
scp -P 11112 opam-1.1.0-bin.tgz jun@49.212.130.159:

export PATH=/app/vendor/opam/bin:$PATH

mkdir /app/vendor/.opam
export OPAMROOT=/app/vendor/.opam
yes N | opam init

eval `opam config env --root=/app/vendor/.opam`

