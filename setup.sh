#!/usr/bin/env bash

set -e

echo PWD=`pwd`
# export PREFIX=$HOME/.share/prefix
# export PATH=$PREFIX/bin:$PATH
# mkdir -p $PREFIX/bin
# mkdir -p $PREFIX/lib
# mkdir -p $PREFIX/share/man

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

# First preparation
opam init -y
# . /app/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

################################################# heroku special repo

git clone https://github.com/camlspotter/opam-repository-heroku.git
mv opam-repository-heroku /app/opam-repository-heroku
# this is to update only heroku
if [ -d /app/.opam/heroku ]; then
  opam repo remove heroku
fi
opam repo add heroku /app/opam-repository-heroku

################################################# build opam
# It fails...
# setup /app http://49.212.130.159:5963/heroku/opam.1.0.1.tgz
# (cd /app/opam; ./configure --prefix /app/.share/prefix; make; make install)

# # opam switch 4.00.1
# # /bin/rm -rf /app/.opam/4.00.1/build
# 
# # opam default has strange behaviour
# 
# opam switch list
# # opam switch 4.00.1+custom
# # opam switch remove 4.00.1

#################################################### build copy image

# /bin/rm -rf /app/.opam/log/*
# /bin/rm -rf /app/.opam/4.00.1+custom/build/*

eval `opam config env`
opam install -y omake
opam install -y spotlib
tar zcvf opam-lib.tgz -C /app .opam

# ########################################## copy which I want to use later
# 
# mkdir -p vendor
# cp -a /app/vendor/ocaml vendor/
# 
# ################################################## copy server
# 
# eval `opam config env`
# opam install -y omake
# opam install -y spotlib
# omake
# mkdir -p target/bin/
# cp main target/bin/main

# opam install -y dbm
# opam install -y eliom

# OPAM repo tweak
# opam repo add opam  http://opam.ocamlpro.com
# git clone https://github.com/camlspotter/opam-repository-heroku.git
# cp -a opam-repository-heroku /app/opam-repository-heroku
# opam repo add heroku /app/opam-repository-heroku
# opam repo remove default
# opam update

# opam switch 4.00.1+custom
# opam switch remove -y 4.00.1
# opam switch remove -y system

# opam install -y dbm
# opam install -y eliom

