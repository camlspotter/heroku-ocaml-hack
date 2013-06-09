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

export LD_LIBRARY_PATH="$BUILD_DIR/runtime/lib:"
export PATH="$BUILD_DIR/runtime/opam/system/bin:$BUILDDIR/runtime/bin:/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin"
export CAML_LD_LIBRARY_PATH=$BUILD_DIR/runtime/opam/system/lib/stublibs
