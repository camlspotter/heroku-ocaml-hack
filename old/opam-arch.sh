#!/bin/sh
tar zcvf opam-lib.tgz .opam .profile .ocamlinit
chmod og+r opam-lib.tgz
scp -P 11112 opam-lib.tgz jun@49.212.130.159:/var/www/heroku/
