#!/bin/bash

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

# opam init -y
# . /app/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
# 
# opam install -y omake
# opam install -y spotlib

setup / http://49.212.130.159:5963/heroku/opam-lib.tgz

. /app/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

omake
mkdir -p target/bin/
cp main target/bin/main

opam system 4.00.1

tar zcf opam-lib.tgz /app/.opam
