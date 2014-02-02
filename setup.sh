#!/usr/bin/env bash

set -e

export sakura=49.212.130.159:5963

echo PWD=`pwd`
export WORK=`pwd`

function setup() {
  dir=$1
  url=$2

  mkdir -p $dir
  echo "fetching $url to $dir"
  curl  $url -s -o - | tar zxf - -C $dir
}

function copy() {
  file=$1
  chmod og+r $1
  scp -P 11112 $1 jun@49.212.130.159:/var/www/heroku/
}

setup /app http://$sakura/heroku/ocaml-4.01.0-bin.tgz
export PATH=/app/vendor/ocaml/bin:$PATH

setup /app http://$sakura/heroku/opam-1.1.0-bin.tgz
export PATH=/app/vendor/opam/bin:$PATH

export OPAMROOT=/app/vendor/.opam
if [ ! -d $OPAMROOT ]; then 
  mkdir /app/vendor/.opam
  yes N | opam init
  opam repo add heroku https://github.com/camlspotter/opam-repository-heroku.git
  opam update
fi
eval `opam config env --root=/app/vendor/.opam`

setup /app https://s3-eu-west-1.amazonaws.com/midgard-heroku/pcre.tar.gz
export PATH="/app/vendor/pcre/bin:$PATH"
export LD_LIBRARY_PATH=/app/vendor/pcre/lib:$LD_LIBRARY_PATH
 
setup /app/vendor/gdbm/ http://49.212.130.159:5963/heroku/gdbm-1.tgz
export PATH="/app/vendor/gdbm/bin:$PATH"
export LD_LIBRARY_PATH=/app/vendor/gdbm/lib:$LD_LIBRARY_PATH

setup /app http://$sakura/heroku/opam-bin.tgz
export PREFIX=/app/vendor/ocaml

cd src
./0fix_ocamlcommon_cmxs
./0fix_ounit_cmxs
