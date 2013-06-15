#!/usr/bin/env bash

set -e

umask 0022

echo PWD=`pwd`
export PREFIX=$HOME/.share/prefix
export PATH=$PREFIX/bin:$PATH
mkdir -p $PREFIX/bin
mkdir -p $PREFIX/lib
mkdir -p $PREFIX/share/man

function setup() {
  dir=$1
  url=$2

  mkdir -p $dir
  echo "fetching $url to $dir"
  curl  $url -s -o - | tar zxf - -C $dir
}

# load 
# setup / http://49.212.130.159:5963/heroku/my-opam-lib.tgz

setup /app https://s3-eu-west-1.amazonaws.com/midgard-heroku/pcre.tar.gz
export PATH="/app/vendor/pcre/bin:$PATH"
export LD_LIBRARY_PATH=/app/vendor/pcre/lib:$LD_LIBRARY_PATH
 
setup /app/vendor/gdbm/ http://49.212.130.159:5963/heroku/gdbm-1.tgz
export PATH="/app/vendor/gdbm/bin:$PATH"
export LD_LIBRARY_PATH=/app/vendor/gdbm/lib:$LD_LIBRARY_PATH

setup /app http://49.212.130.159:5963/heroku/ocaml-4.00.1-custom.tgz
export PATH="/app/vendor/ocaml/bin:$PATH"

setup /app http://49.212.130.159:5963/heroku/opam.tgz
export PATH="/app/vendor/opam/bin:$PATH"
