#!/usr/bin/env bash

set -e

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
setup / http://49.212.130.159:5963/heroku/my-opam-lib.tgz

# First preparation
# opam init -y
. /app/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

eval `opam config env`
opam install -y omake
opam install -y spotlib
omake
mkdir -p target/bin/
cp main target/bin/main

opam switch 4.00.1
/bin/rm -rf /app/.opam/4.00.1/build

tar zcvf opam-lib-4.00.1.tgz /app/.opam
