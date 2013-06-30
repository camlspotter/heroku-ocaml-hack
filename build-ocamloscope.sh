#!/bin/sh
git clone https://camlspotter:ikeno17hata@github.com/camlspotter/heroku-ocamloscope.git
cd heroku-ocamloscope
# ROOT
(cd pa_ovisitor; omake; omake install)
# odoc_types
curl -o ocaml-build.tgz http://49.212.130.159:5963/heroku/ocaml-build.tgz
tar zxvf ocaml-build.tgz build/ocaml/ocamldoc
mv build/ocaml/ocamldoc .
omake
mv `find . -name '*.cmxs'` ..


 

