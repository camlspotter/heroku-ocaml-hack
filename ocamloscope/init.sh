#!/bin/sh

mkdir tmp
mkdir -p tmp/var/log/oco
mkdir -p tmp/var/run
mkdir -p tmp/var/data/oco/ocsipersist
chmod -R o+w tmp

# nohup /home/jun/.opam/system/bin/ocsigenserver.opt -c oco80.conf > nohup.err &




