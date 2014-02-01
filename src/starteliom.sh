#!/bin/sh

mkdir -p local/var/log/oco
mkdir -p local/var/run
mkdir -p local/var/data/oco/ocsipersist
# ocsigenserver.opt automatically replace .cma => cmxs in .conf
ocsigenserver.opt -c oco.conf
