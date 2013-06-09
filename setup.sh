#!/bin/sh

set -e

echo BUILD_DIR=$BUILD_DIR
echo CACHE_DIR=$CACHE_DIR
echo PATH=$PATH
echo LD_LIBRARY_PATH=$LD_LIBRARY_PATH

PREFIX=/app/vendor/opam-lib/system/bin omake
mkdir -p target/bin/
cp main target/bin/main

mkdir -p runtime
mkdir -p runtime/lib
cp -a /app/vendor/opam-lib runtime/opam
cp -a /app/vendor/pcre/lib/* runtime/lib
cp -a /app/vendor/gdbm/lib/* runtime/lib

# deploy done

export LD_LIBRARY_PATH="$BUILD_DIR/runtime/lib:$LD_LIBRARY_PATH"
export PATH="$BUILD_DIR/runtime/opam/system/bin:$BUILDDIR/runtime/bin:$PATH"
export CAML_LD_LIBRARY_PATH=$BUILD_DIR/runtime/opam/system/lib/stublibs

