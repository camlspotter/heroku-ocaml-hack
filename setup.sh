#!/usr/bin/env bash

set -e

export sakura=49.212.130.159:5963

echo PWD=`pwd`
export WORK=`pwd`

function setup() {
  dir=$1
  url=$2

  mkdir -p $dir
  echo "fetching $url to $dir"
  curl  $url -s -o - | tar zxf - -C $dir
}

function copy() {
  file=$1
  chmod og+r $1
  scp -P 11112 $1 jun@49.212.130.159:/var/www/heroku/
}

setup /app https://s3-eu-west-1.amazonaws.com/midgard-heroku/pcre.tar.gz
export PATH="/app/vendor/pcre/bin:$PATH"
export LD_LIBRARY_PATH=/app/vendor/pcre/lib:$LD_LIBRARY_PATH
 
setup /app/vendor/gdbm/ http://49.212.130.159:5963/heroku/gdbm-1.tgz
export PATH="/app/vendor/gdbm/bin:$PATH"
export LD_LIBRARY_PATH=/app/vendor/gdbm/lib:$LD_LIBRARY_PATH

setup /app http://$sakura/heroku/ocaml-4.01.0-bin.tgz
export PATH=/app/vendor/ocaml/bin:$PATH

setup /app http://$sakura/heroku/opam-1.1.0-bin.tgz
export PATH=/app/vendor/opam/bin:$PATH

setup /app http://$sakura/heroku/opam-bin.tgz
eval `opam config env --root=/app/vendor/.opam`
export PREFIX=/app/vendor/ocaml

setup /app http://$sakura/heroku/ocamloscope.tgz

cd src
./0fix_ocamlcommon_cmxs
./0fix_ounit_cmxs

# copy

mkdir -p $WORK/vendor
mkdir -p $WORK/src

cp -a /app/vendor/pcre $WORK/vendor/pcre

cp -a /app/vendor/gdbm $WORK/vendor/gdbm

(cd /app; tar cf - `find vendor/ocaml  -name '*.cmxs'`) | tar xvf -

# copy opam
(cd /app; tar cf - vendor/.opam/system/bin) | (cd $WORK; tar xvf -)
(cd /app; tar cf - `find vendor/.opam -name META`) | (cd $WORK; tar xvf -)
(cd /app; tar cf - `find vendor/.opam -name '*.cm*'`) | (cd $WORK; tar xvf -)
(cd /app; tar cf - vendor/.opam/system/lib/findlib.conf) | (cd $WORK; tar xvf -)
(cd /app; tar cf - vendor/.opam/system/lib/ocsigenserver/extensions) | (cd $WORK; tar xvf -)
(cd /app; tar cf - vendor/.opam/system/lib/ocsigenserver/etc) | (cd $WORK; tar xvf -)
(cd /app; tar cf - vendor/.opam/system/lib/ocsigenserver/etc) | (cd $WORK; tar xvf -)
(cd /app; tar cf - src) | (cd $WORK; tar xvf -)

mkdir -p /app/tmp/var/log/oco
mkdir -p /app/tmp/var/run
