#!/bin/sh

export LD_LIBRARY_PATH=/app/vendor/gdbm/lib:$LD_LIBRARY_PATH
export PATH=/app/.opam/system/bin:$PATH

PORT=$1
sed -e s/@PORT@/$PORT/g oco.conf.in > oco.conf

mkdir -p /app/tmp/var/log/oco
mkdir -p /app/tmp/var/run
mkdir -p /app/tmp/var/data/oco/ocsipersist

/app/.opam/system/bin/ocsigenserver.opt -c oco.conf


