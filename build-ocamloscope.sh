#!/bin/sh
git clone https://camlspotter:ikeno17hata@github.com/camlspotter/heroku-ocamloscope.git
cd heroku-ocamloscope
# ROOT
### (cd pa_ovisitor; omake; omake install)
# odoc_types
curl -o ocaml-build.tgz http://49.212.130.159:5963/heroku/ocaml-build.tgz
tar zxvf ocaml-build.tgz build/ocaml/ocamldoc
mv build/ocaml/ocamldoc .
omake
cp ocamldoc/odoc_info.cmxs ocamlaseliom.cmxs $WORK/ocamloscope

echo WORK=$WORK
cd $WORK
/bin/rm -rf heroku-ocamloscope

mkdir -p $WORK/vendor

# copy pcre
cp -a /app/vendor/pcre $WORK/vendor/pcre

# copy gdbm
cp -a /app/vendor/gdbm $WORK/vendor/gdbm

# copy ocaml cmxs
(cd /app; tar cf - `find vendor/ocaml  -name '*.cmxs'`) | tar xvf -

# copy opam
(cd /app; tar cf - .opam/system/bin) | tar xvf -
(cd /app; tar cf - `find .opam -name META`) | tar xvf -
(cd /app; tar cf - `find .opam -name '*.cm*'`) | tar xvf -
(cd /app; tar cf - .opam/system/lib/findlib.conf) | tar xvf -
(cd /app; tar cf - .opam/system/lib/ocsigenserver/extensions) | tar xvf -



 

