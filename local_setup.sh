#!/usr/bin/env bash

# heroku run bash
# . local_setup.sh

export sakura=49.212.130.159:5963

echo PWD=`pwd`
export WORK=`pwd`
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

function copy() {
  file=$1
  chmod og+r $1
  scp -P 11112 $1 jun@49.212.130.159:/var/www/heroku/
}

# load 
# setup / http://49.212.130.159:5963/heroku/my-opam-lib.tgz

setup /app http://$sakura/heroku/ocaml-4.01.0-bin.tgz
export PATH=/app/vendor/ocaml/bin:$PATH

setup /app http://$sakura/heroku/opam-1.1.0-bin.tgz
export PATH=/app/vendor/opam/bin:$PATH

export OPAMROOT=/app/vendor/.opam
if [ ! -d $OPAMROOT ]; then 
  mkdir /app/vendor/.opam
  yes N | opam init
  opam repo add heroku https://github.com/camlspotter/opam-repository-heroku.git
  opam update
fi
eval `opam config env --root=/app/vendor/.opam`

setup /app https://s3-eu-west-1.amazonaws.com/midgard-heroku/pcre.tar.gz
export PATH="/app/vendor/pcre/bin:$PATH"
export LD_LIBRARY_PATH=/app/vendor/pcre/lib:$LD_LIBRARY_PATH
 
setup /app/vendor/gdbm/ http://49.212.130.159:5963/heroku/gdbm-1.tgz
export PATH="/app/vendor/gdbm/bin:$PATH"
export LD_LIBRARY_PATH=/app/vendor/gdbm/lib:$LD_LIBRARY_PATH

setup /app http://$sakura/heroku/opam-bin.tgz
export PATH="/app/vendor/.opam/system/bin:$PATH"
export PREFIX=/app/vendor/ocaml

# opam : spotlib orakuda treeprint tiny_json_conv dbm eliom pa_ovisitor
# 0fix_ocamlcommon_cmxs
# 0fix_ounit_cmxs
# (cd /app; tar zcvf ~/src/ocaml-cmxs.tgz vendor/ocaml/lib/ocaml/compiler-libs/ocamlcommon.cmxs vendor/ocaml/lib/ocaml/ocamldoc/odoc_info.cmxs vendor/.opam/system/lib/oUnit/oUnit.cmxs)

# setup /app http://49.212.130.159:5963/heroku/opam-lib.tgz
# 
# # # . /app/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
# # 
# # ################################################# heroku special repo
# # 
# # git clone https://github.com/camlspotter/opam-repository-heroku.git
# # mv opam-repository-heroku /app/opam-repository-heroku
# # # this is to update only heroku
# # if [ -d /app/.opam/heroku ]; then
# #   opam repo remove heroku
# # fi
# # opam repo add heroku /app/opam-repository-heroku
# 
# ################################################# build opam
# # It fails...
# # setup /app http://49.212.130.159:5963/heroku/opam.1.0.1.tgz
# # (cd /app/opam; ./configure --prefix /app/.share/prefix; make; make install)
# 
# # # opam switch 4.00.1
# # # /bin/rm -rf /app/.opam/4.00.1/build
# # 
# # # opam default has strange behaviour
# # 
# # opam switch list
# # # opam switch 4.00.1+custom
# # # opam switch remove 4.00.1
# 
# #################################################### build copy image
# 
# # /bin/rm -rf /app/.opam/log/*
# # /bin/rm -rf /app/.opam/4.00.1+custom/build/*
# 
# eval `opam config env`
# opam install -y omake
# opam install -y spotlib
# # opam install treeprint orakuda CamlGI meta_conv pa_ovisitor
# # tar zcvf opam-lib.tgz -C /app .opam
# 
# # ########################################## copy which I want to use later
# # 
# # mkdir -p vendor
# # cp -a /app/vendor/ocaml vendor/
# # 
# # ################################################## copy server
# # 
# # eval `opam config env`
# # opam install -y omake
# # opam install -y spotlib
# mkdir -p $HOME/.share/prefix
# export PREFIX=$HOME/.share/prefix
# omake
# mkdir -p target/bin/
# cp main target/bin/main
# 
# # opam install -y dbm
# # opam install -y eliom
# 
# # OPAM repo tweak
# # opam repo add opam  http://opam.ocamlpro.com
# # git clone https://github.com/camlspotter/opam-repository-heroku.git
# # cp -a opam-repository-heroku /app/opam-repository-heroku
# # opam repo add heroku /app/opam-repository-heroku
# # opam repo remove default
# # opam update
# 
# # opam switch 4.00.1+custom
# # opam switch remove -y 4.00.1
# # opam switch remove -y system
# 
# # opam install -y dbm
# # opam install -y eliom
# 
# ./build-ocamloscope.sh
