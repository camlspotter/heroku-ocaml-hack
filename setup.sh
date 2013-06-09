#!/bin/sh

set -e

PWD=`pwd`
echo PWD=$PWD
echo PATH=$PATH
echo LD_LIBRARY_PATH=$LD_LIBRARY_PATH

PREFIX=/app/vendor/opam-lib/system/bin omake

mkdir -p runtime
mkdir -p runtime/lib
cp -a /app/vendor/opam-lib runtime/opam
cp -a /app/vendor/pcre/lib/* runtime/lib
cp -a /app/vendor/gdbm/lib/* runtime/lib

# deploy done

export RUNTIME=$PWD/runtime
export LD_LIBRARY_PATH="$RUNTIME/lib:"
export PATH="$RUNTIME/opam/system/bin:$RUNTIME/bin:/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin"
export CAML_LD_LIBRARY_PATH=$RUNTIME/opam/system/lib/stublibs
export OCAMLFIND_CONF=$RUNTIME/findlib.conf
