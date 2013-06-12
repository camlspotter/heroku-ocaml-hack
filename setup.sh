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

# # included in my-opam-lib.tgz
setup /app https://s3-eu-west-1.amazonaws.com/midgard-heroku/pcre.tar.gz
export PATH="/app/vendor/pcre/bin:$PATH"
export LD_LIBRARY_PATH=/app/vendor/pcre/lib:$LD_LIBRARY_PATH
 
# # included in my-opam-lib.tgz
setup /app/vendor/gdbm/ http://49.212.130.159:5963/heroku/gdbm-1.tgz
export PATH="/app/vendor/gdbm/bin:$PATH"
export LD_LIBRARY_PATH=/app/vendor/gdbm/lib:$LD_LIBRARY_PATH

# First preparation
# opam init -y
# . /app/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

opam switch 4.00.1
eval `opam config env`
opam install -y omake
opam install -y spotlib

omake
mkdir -p target/bin/
cp main target/bin/main

# opam switch 4.00.1
# /bin/rm -rf /app/.opam/4.00.1/build

# opam default has strange behaviour

# OPAM repo tweak
# opam repo add opam   http://opam.ocamlpro.com
# git clone https://github.com/camlspotter/opam-repository-heroku.git
# cp -a opam-repository-heroku /app/opam-repository-heroku
# opam repo add heroku /app/opam-repository-heroku
# opam repo remove default
# opam update
# opam repo

# OPAM repo tweak
# opam repo add opam  http://opam.ocamlpro.com
git clone https://github.com/camlspotter/opam-repository-heroku.git
cp -a opam-repository-heroku /app/opam-repository-heroku
# opam repo add heroku /app/opam-repository-heroku
# opam repo remove default
opam update
opam switch 4.00.1+custom
opam switch remove -y 4.00.1
opam switch remove -y system

# opam install -y dbm

/bin/rm -rf /app/.opam/log/*
/bin/rm -rf /app/.opam/4.00.1
/bin/rm -rf /app/.opam/system
/bin/rm -rf /app/.opam/4.00.1+custom/build/*
tar zcf opam-lib.tgz -C /app .opam .share vendor/pcre vendor/gdbm
tar zvf opam-lib.tgz
