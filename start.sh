#!/bin/sh

set -e

PORT=$1

export RUNTIME=$PWD/runtime
export LD_LIBRARY_PATH="$RUNTIME/lib:"
export PATH="$RUNTIME/opam/system/bin:$RUNTIME/bin:/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin"
export CAML_LD_LIBRARY_PATH=$RUNTIME/opam/system/lib/stublibs
export OCAMLFIND_CONF=$RUNTIME/findlib.conf
runtime/opam/system/bin/ocsigenserver.opt -c graffiti-test.conf

