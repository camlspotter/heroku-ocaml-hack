#!/bin/sh

echo pwd=`pwd`
echo find vendor
find vendor -maxdepth 1 | head -100

cd build

export LD_LIBRARY_PATH=/app/vendor/gdbm/lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH=/app/vendor/pcre/lib:$LD_LIBRARY_PATH
export PATH=/app/.opam/system/bin:$PATH

PORT=$1
sed -e s/@PORT@/$PORT/g oco.conf.in > oco.conf

head oco.conf

mkdir -p /app/tmp/var/log/oco
mkdir -p /app/tmp/var/run
mkdir -p /app/tmp/var/data/oco/ocsipersist

/app/vendor/.opam/system/bin/ocsigenserver.opt -v -c oco.conf
