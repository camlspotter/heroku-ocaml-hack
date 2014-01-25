#!/usr/bin/env bash

#heroku run bash 

function save() {
    path=$1
    dir=$2
    tar zcvf $path $dir
    chmod og+r $path
    scp -P 11112 $path jun@49.212.130.159:/var/www/heroku
}

set -e

curl -o ocaml-4.01.0.tar.gz http://caml.inria.fr/pub/distrib/ocaml-4.01/ocaml-4.01.0.tar.gz
tar zxvf ocaml-4.01.0.tar.gz
cd ocaml-4.01.0
./configure --prefix /app/vendor/ocaml
make world.opt install
 
export PATH=/app/vendor/ocaml/bin:$PATH

cd
curl -o opam-full-1.1.0.tar.gz http://www.ocamlpro.com/pub/opam-full-1.1.0.tar.gz
tar zxvf opam-full-1.1.0.tar.gz
cd opam-full-1.1.0
./configure --prefix /app/vendor/opam
make
make install
cd
tar zcvf opam-1.1.0-bin.tgz vendor/opam

export PATH=/app/vendor/opam/bin:$PATH

mkdir /app/vendor/.opam
export OPAMROOT=/app/vendor/.opam
yes N | opam init

eval `opam config env --root=/app/vendor/.opam`

save vendor/ocaml ocaml-4.01.0-bin.tgz
save vendor/opam opam-1.1.0-bin.tgz
