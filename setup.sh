#!/bin/sh

set -e

PREFIX=/app/vendor/opam-lib/system/bin omake
mkdir -p target/bin/
cp main target/bin/main

mkdir -p /app/opam
cp -a /app/vendor/opam-lib/system opam
