#!/bin/sh

set -e

echo BUILDDIR=$BUILDDIR
echo PATH=$PATH
echo LD_LIBRARY_PATH=$LD_LIBRARY_PATH

PREFIX=/app/vendor/opam-lib/system/bin omake
mkdir -p target/bin/
cp main target/bin/main

mkdir -p /app/runtime
mkdir -p /app/runtime/lib
cp -a /app/vendor/opam-lib runtime/opam
cp -a /app/vendor/pcre/lib/* runtime/lib
cp -a /app/vendor/gdbm/lib/* runtime/lib

# deploy done

export LD_LIBRARY_PATH="$BUILDDIR/runtime/lib:$LD_LIBRARY_PATH"
export PATH="$BUILDDIR/runtime/opam/system/bin:$BUILDDIR/runtime/bin:$PATH"
export CAML_LD_LIBRARY_PATH=$BUILDDIR/runtime/opam/system/lib/stublibs

